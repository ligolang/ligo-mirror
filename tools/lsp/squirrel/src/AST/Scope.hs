
{-# language Strict #-}

{- | /The/ scope resolution system.
-}

module AST.Scope
  -- ( HasLocalScope (..)
  -- , addLocalScopes
  -- , lookupEnv
  -- , Kind (..)
  -- , ScopedDecl (..)
  -- )
  where

import           Control.Arrow (second)
import           Control.Monad.State
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import           Control.Monad.Writer (WriterT, execWriterT, tell)

import           Data.Monoid         (First (..))
import qualified Data.List   as List
import           Data.Map            (Map)
import qualified Data.Map    as Map
import           Data.Maybe          (listToMaybe, maybeToList)
import           Data.Text           (Text)
import           Data.Foldable       (toList, for_)

import           Duplo.Lattice
import           Duplo.Pretty
import           Duplo.Tree hiding (loop)
import           Duplo.Error

import           AST.Skeleton
import           AST.Parser
import           Parser
import           Product
import           Range

import           Debug.Trace

-- type CollectM = StateT (Product [FullEnv, [Range], Maybe ScopedDecl]) Catch

type FullEnv = Product ["vars" := Env, "types" := Env]
type Env     = Map Range [ScopedDecl]

data Category = Variable | Type
  deriving Eq

-- | The type/value declaration.
data ScopedDecl = ScopedDecl
  { _sdName    :: Text
  , _sdOrigin  :: Range
  , _sdBody    :: Maybe Range
  , _sdType    :: Maybe (Either (LIGO ()) Kind)
  , _sdRefs    :: [Range]
  , _sdDoc     :: [Text]
  }
  deriving Show via PP ScopedDecl

instance Eq ScopedDecl where
  sd == sd1 = and
    [ pp (_sdName   sd) == pp (_sdName   sd1)
    ,     _sdOrigin sd  ==     _sdOrigin sd1
    ]

-- | The kind.
data Kind = Star
  deriving Show via PP Kind

instance {-# OVERLAPS #-} Pretty FullEnv where
  pp = block . map aux . Map.toList . mergeFE
    where
      aux (r, fe) =
        pp r `indent` block fe

      mergeFE fe = getTag @"vars" @Env fe Prelude.<> getTag @"types" fe

instance Pretty ScopedDecl where
  pp (ScopedDecl n o _ t refs doc) =
    sexpr "decl" [pp n, pp o, pp t, pp refs, pp doc]

instance Pretty Kind where
  pp _ = "TYPE"

instance Pretty Category where
  pp Variable = "Variable"
  pp Type     = "Type"

emptyEnv :: FullEnv
emptyEnv = Tag Map.empty :> Tag Map.empty :> Nil

with :: Category -> FullEnv -> (Env -> Env) -> FullEnv
with Variable env f = modTag @"vars"  f env
with Type     env f = modTag @"types" f env

ofCategory :: Category -> ScopedDecl -> Bool
ofCategory Variable ScopedDecl { _sdType = Just (Right Star) } = False
ofCategory Variable _                                          = True
ofCategory Type     ScopedDecl { _sdType = Just (Right Star) } = True
ofCategory _        _                                          = False

type Info' = Product [[ScopedDecl], Maybe Category, [Text], Range, ShowRange]

newtype ScopeForest = ScopeForest
  { unScopeForest :: [Tree '[[]] ScopeInfo]
  }

type ScopeInfo = Product [[ScopedDecl], Range]

withScopeForest f (ScopeForest sf) = ScopeForest (f sf)

instance {-# OVERLAPPABLE #-} Pretty x => Show x where
  show = show . pp

instance Pretty ScopeForest where
  pp (ScopeForest sf) = go sf
    where
      go = sexpr "list" . map go'
      go' :: Tree' '[[]] '[[ScopedDecl], Range] -> Doc
      go' (only -> (decls :> r :> Nil, list)) =
        sexpr "scope" ([pp r] ++ map pp decls ++ [go list | not $ null list])

addLocalScopes :: LIGO Info -> LIGO Info'
addLocalScopes tree
  = either (error . show) id
  $ runCatch
  $ descent @Info @Info' @RawLigoList @RawLigoList defaultHandler
      [ Descent \(i, Name t) -> do
          let env = envAtPoint (getRange i) forest
          return (env :> Just Variable :> i, Name t)

      , Descent \(i, NameDecl t) -> do
          let env = envAtPoint (getRange i) forest
          return (env :> Just Variable :> i, NameDecl t)

      , Descent \(i, TypeName t) -> do
          let env = envAtPoint (getRange i) forest
          return (env :> Just Type :> i, Name t)
      ]
      tree
  where
    defaultHandler f (i :< fs) = do
      fs' <- traverse f fs
      let env = envAtPoint (getRange i) forest
      return ((env :> Nothing :> i) :< fs')

    forest = getEnv tree

lookupEnv :: Text -> [ScopedDecl] -> Maybe ScopedDecl
lookupEnv name = getFirst . foldMap \decl ->
  First do
    guard (_sdName decl == name)
    return decl

envAtPoint :: Range -> ScopeForest -> [ScopedDecl]
envAtPoint r (ScopeForest sf) = do
  tree  <- sf
  reverse $ spine (leq r . getRange) tree
  where
    spine isR (only -> (i, trees)) = if
      | isR i     -> getElem @[ScopedDecl] i <> foldMap (spine isR) trees
      | otherwise -> []

addReferences :: LIGO Info -> ScopeForest -> ScopeForest
addReferences ligo = execState $ loopM_ addRef ligo
  where
    addRef = \case
      (match -> Just (r, Name n)) -> do
        modify
          $ withScopeForest
          $ map
          $ findAndUpdateFrom (leq (getRange r) . getRange)
          $ traverseElem @[ScopedDecl]
          $ walkScopeName r n

      (match -> Just (r, TypeName n)) -> do
        modify
          $ withScopeForest
          $ map
          $ findAndUpdateFrom (leq (getRange r) . getRange)
          $ traverseElem @[ScopedDecl]
          $ walkScopeTypeName r n

      _ -> do
        return ()

    walkScopeName r n (decl : rest) = if
      | _sdName decl == n
      , maybe True isLeft (_sdType decl)
        -> Just $ decl { _sdRefs = getRange r : _sdRefs decl } : rest

      | otherwise -> (decl :) <$> walkScopeName r n rest

    walkScopeName r n [] = Nothing

    walkScopeTypeName r n (decl : rest) = if
      | _sdName decl == n
      , maybe True (not . isLeft) (_sdType decl)
        -> Just $ decl { _sdRefs = getRange r : _sdRefs decl } : rest

      | otherwise -> (decl :) <$> walkScopeTypeName r n rest

    walkScopeTypeName r n [] = Nothing

ignoreFailure :: (a -> Maybe a) -> (a -> Maybe a)
ignoreFailure f a = maybe (Just a) Just (f a)

isLeft Left {} = True
isLeft _       = False

getEnv :: LIGO Info -> ScopeForest
getEnv tree
  = addReferences tree
  $ ScopeForest
  $ compressScopeTree . pure
  $ extractScopeTree
  $ prepareTree
    tree

prepareTree
  = assignDecls
  . unSeq
  . unLetRec

loop go = aux
  where
    aux (r :< fs) = go $ r :< fmap aux fs

loopM_ :: (Monad m, Foldable f) => (Cofree f a -> m ()) -> (Cofree f a -> m ())
loopM_ go = aux
  where
    aux (r :< fs) = do
      for_ fs aux
      go $ r :< fs

-- funsToLambdas
--   :: ( Contains  Range     xs
--      , Contains [Text]     xs
--      , Contains  ShowRange xs
--      , Eq (Product xs)
--      )
--   => LIGO' xs
--   -> LIGO' xs
-- funsToLambdas = loop go
--   where
--     go = \case
--       (match -> Just (r, Let (match -> Just (r', Function _ n a ty b)) body)) -> do


unLetRec
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     )
  => LIGO' xs
  -> LIGO' xs
unLetRec = loop go
  where
    go = \case
      (match -> Just (r, expr)) -> do
        case expr of
          Let (match -> Just (r', Seq decls)) body -> do
            foldr reLet body decls
          _ -> make (r, expr)

      -- TODO: somehow append Unit to the end
      (match -> Just (r, RawContract decls)) -> do
        case reverse decls of
          lst : (reverse -> ini) -> do
            foldr reLet lst ini

      it -> it

unSeq
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     )
  => LIGO' xs
  -> LIGO' xs
unSeq = loop go
  where
    go = \case
      (match -> Just (r, Seq decls)) -> do
        case reverse decls of
          lst : (reverse -> ini) -> do
            foldr reLet lst ini

      it -> it

reLet decl body = make (r', Let decl body)
  where
    r' = putElem (getRange decl `merged` getRange body)
        $ extract body

assignDecls
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     , Pretty (Product xs)
     )
  => LIGO' xs
  -> LIGO' ([ScopedDecl] : Bool : Range : xs)
assignDecls = loop go . fmap (\r -> [] :> False :> getRange r :> r)
  where
    go = \case
      (match -> Just (r, Let decl body)) -> do
        let imm = getImmediateDecls decl
        let r' :< body' = body
        let l' :< decl' = decl
        let r'' = putElem (getRange body) $ putElem True $ modElem (imm <>) r'
        let l'' = putElem (getRange decl) $ putElem True l'
        make (r, Let (l'' :< decl') (r'' :< body'))

      (match -> Just (r, Lambda args ty body)) -> do
        let imms = getImmediateDecls =<< args
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        make (r, Lambda args ty (r'' :< body'))

      (match -> Just (r, Alt pat body)) -> do
        let imms = getImmediateDecls pat
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        make (r, Alt pat (r'' :< body'))

      (match -> Just (r, Function True n a ty b)) -> do
        let imms = getImmediateDecls =<< a
        let
          f = ScopedDecl
            { _sdName   = ppToText n
            , _sdOrigin = getRange n
            , _sdBody   = Just $ getRange b
            , _sdType   = (Left . void) <$> ty
            , _sdRefs   = []
            , _sdDoc    = getElem r
            }
        let r'   = putElem True $ modElem ((f : imms) <>) r
        make (r', Function True n a ty b)

      (match -> Just (r, Function False n a ty b)) -> do
        let imms = getImmediateDecls =<< a
        let r' :< body = b
        let r'' = putElem True $ modElem (imms <>) r'
        let r''' = putElem True r
        make (r''', Function False n a ty (r'' :< body))

      it -> it

extractScopeTree
  :: LIGO' ([ScopedDecl] : Bool : Range : xs)
  -> Tree' '[[]] '[[ScopedDecl], Bool, Range]
extractScopeTree = go
  where
    go
      :: LIGO' ([ScopedDecl] : Bool : Range : xs)
      -> Tree' '[[]] '[[ScopedDecl], Bool, Range]
    go ((decls :> visible :> r :> _) :< fs) = do
      let fs'  = toList fs
      let fs'' = map go fs'
      make (decls :> visible :> r :> Nil, fs'')

compressScopeTree
  :: [Tree' '[[]] '[[ScopedDecl], Bool, Range]]
  -> [Tree' '[[]] '[[ScopedDecl], Range]]
compressScopeTree = (go =<<)
  where
    go
      :: Tree' '[[]] '[[ScopedDecl], Bool, Range]
      -> [Tree' '[[]] '[[ScopedDecl], Range]]
    go (only -> (decls :> False :> r :> Nil, list)) =
      list >>= go

    go (only -> (decls :> True :> r :> Nil, list)) = do
      let list' = list >>= go
      [ make (decls :> r :> Nil, list')
       | not (null decls) || not (null list')
       ]

getImmediateDecls
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     )
  => LIGO' xs
  -> [ScopedDecl]
getImmediateDecls = \case
  (match -> Just (r, pat)) -> do
    case pat of
      IsVar v -> do
        let (r', name) = getName v
        [ScopedDecl name r' Nothing Nothing [] (getElem r)]

      IsTuple xs -> getImmediateDecls =<< xs
      IsList  xs -> getImmediateDecls =<< xs
      IsSpread s -> getImmediateDecls s
      IsWildcard -> []
      IsAnnot x t -> getImmediateDecls x
      IsCons h t -> getImmediateDecls h <> getImmediateDecls t
      IsConstant _ -> []
      IsConstr _ xs -> getImmediateDecls =<< maybeToList xs

  (match -> Just (r, pat)) -> do
    case pat of
      Irrefutable  irr _ -> getImmediateDecls irr
      Function     _ f _ t b -> do
        let (r', name) = getName f
        [ScopedDecl name r' (Just $ getRange b) (Left . void <$> t) [] (getElem r)]

      Var v t b -> do
        let (r', name) = getName v
        [ScopedDecl name r' (getRange <$> b) (Left . void <$> t) [] (getElem r)]

      Const c t b -> do
        let (r', name) = getName c
        [ScopedDecl name r' (getRange <$> b) (Left . void <$> t) [] (getElem r)]

      TypeDecl t b -> do
        let (r', name) = getTypeName t
        [ScopedDecl name r' (Just $ getRange b) kind [] (getElem r)]

      Attribute _ -> []
      Include _ -> []

  (match -> Just (r, Parameters ps)) -> do
    ps >>= getImmediateDecls

  _ -> []

kind = Just $ Right Star

select
  :: ( Lattice  (Product info)
     , Contains  ShowRange info
     , Contains  Range info
     , Modifies (Product info)
     , Eq (Product info)
     )
  => Text
  -> [Visit RawLigoList (Product info) (WriterT [LIGO (Product info)] Catch)]
  -> LIGO (Product info)
  -> (Range, Text)
select what handlers t
  = maybe
      (error . show $ "Tree does not contain a" <+> pp what <.> ":" <+> pp t <+> pp (getRange $ extract t))
      (\t -> (getElem $ extract t, ppToText t))
  $ either (const Nothing) listToMaybe
  $ runCatch
  $ execWriterT
  $ visit handlers
    t

getName
  :: ( Lattice  (Product info)
     , Contains  ShowRange info
     , Contains  Range info
     , Modifies (Product info)
     , Eq (Product info)
     )
  => LIGO (Product info)
  -> (Range, Text)
getName = select "name"
  [ Visit \(r, NameDecl t) -> do
      tell [make (r, Name t)]
  ]

getTypeName
  :: ( Lattice  (Product info)
     , Contains  ShowRange info
     , Contains  Range info
     , Modifies (Product info)
     , Eq (Product info)
     )
  => LIGO (Product info)
  -> (Range, Text)
getTypeName = select "type name"
  [ Visit \(r, TypeName t) -> do
      tell [make (r, TypeName t)]
  ]