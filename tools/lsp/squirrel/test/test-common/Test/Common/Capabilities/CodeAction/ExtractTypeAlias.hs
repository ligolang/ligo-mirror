module Test.Common.Capabilities.CodeAction.ExtractTypeAlias
  ( extractTypeAliasDriver
  , extractTextEdits
  , testCases
  , constructExpectedWorkspaceEdit
  , testInfos
  , TestInfo (..)
  ) where

import Control.Lens
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Language.LSP.Types qualified as J
import Language.LSP.Types.Lens qualified as J
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import AST.Capabilities.CodeAction.ExtractTypeAlias
import Range

import Test.Common.Capabilities.Util (contractsDir)
import Test.Common.FixedExpectations (shouldBe)
import Test.Common.Util (ScopeTester, readContractWithScopes)

data TestInfo = TestInfo
  { tiContract :: String
  , tiCursor :: Range
  , tiExpectedEdits :: [(Range, String)] -- [(range, newText)]
  }

mkr :: J.UInt -> J.UInt -> J.UInt -> J.UInt -> Range
mkr sl sc rl rc = Range (sl, sc, 0) (rl, rc, 0) ""

testInfos :: [TestInfo]
testInfos =
  [ TestInfo
    { tiContract = "simple.ligo"
    , tiCursor = point 2 24
    , tiExpectedEdits =
        [ (mkr 1 29 1 32 , extractedTypeNameAlias)
        , (mkr 2 23 2 26 , extractedTypeNameAlias)
        , (mkr 2 29 2 32 , extractedTypeNameAlias)
        , (mkr 3 13 3 16 , extractedTypeNameAlias)
        , (mkr 1  1 1  1 , "type " <> extractedTypeNameAlias <> " is nat\n")
        ]
    }
  , TestInfo
    { tiContract = "parametric.mligo"
    , tiCursor = interval 1 22 35
    , tiExpectedEdits =
        [ (mkr 1 22 1 35 , extractedTypeNameAlias)
        , (mkr 1  1 1  1 , "type ('a, 'b) " <> extractedTypeNameAlias <> " = int * 'a * 'b\n")
        ]
    }
  , TestInfo
    { tiContract = "existential.mligo"
    , tiCursor = interval 1 9 17
    , tiExpectedEdits =
        [ (mkr 1 9 1 17 , extractedTypeNameAlias)
        , (mkr 1 1 1  1 , "type ('a, 'b) " <> extractedTypeNameAlias <> " = 'a -> 'b\n")
        ]
    }
  ]

constructExpectedWorkspaceEdit :: [(Range, String)] -> [J.TextEdit]
constructExpectedWorkspaceEdit = map constructCodeAction
  where
    constructCodeAction (r, s) = J.TextEdit { _range = toLspRange r , _newText = T.pack s }

extractTypeAliasDriver :: TestTree -> TestTree
extractTypeAliasDriver = testGroup "Extract type extractedTypeNameAlias code action" . pure

testCases :: forall parser. ScopeTester parser => [TestTree]
testCases = map (makeTestCase @parser) testInfos

makeTestCase :: forall parser. ScopeTester parser => TestInfo -> TestTree
makeTestCase testInfo = testCase (tiContract testInfo) (makeTest @parser testInfo)

extractTextEdits :: J.CodeAction -> [J.TextEdit]
extractTextEdits action = unwrapEdits edits
  where
    edits :: [(J.Uri, J.List J.TextEdit)]
    edits = action ^. J.edit . _Just . J.changes . _Just . to HM.toList

    unwrapEdits :: [(J.Uri, J.List J.TextEdit)] -> [J.TextEdit]
    unwrapEdits = \case
      [] -> []
      [(_, J.List e)] -> e
      _ -> error "unwrapEdits: malformed list"

makeTest :: forall parser. ScopeTester parser => TestInfo -> Assertion
makeTest TestInfo{tiContract, tiCursor, tiExpectedEdits} = do
  contractPath <- makeAbsolute $ contractsDir </> "code-action" </> "extract-type-definition" </> tiContract
  tree <- readContractWithScopes @parser contractPath
  let [action] = typeExtractionCodeAction tiCursor (J.filePathToUri contractPath) tree
  let resultingEdits = extractTextEdits action
  resultingEdits `shouldBe` constructExpectedWorkspaceEdit tiExpectedEdits
