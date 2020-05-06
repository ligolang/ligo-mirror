
{- TODO(kirill.andreev): add offsets to ranges, store verbatim in Wrong* -}

module AST.Types where

import Control.Monad.State

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void

import Parser
import ParseTree
import Pretty

import Debug.Trace

type TODO = Text

data Contract info
  = Contract      info [Declaration info]
  | WrongContract      Error
  deriving (Show) via PP (Contract info)

instance Stubbed (Contract info) where stub = WrongContract

data Declaration info
  = ValueDecl info (Binding info)
  | TypeDecl  info (Name info) (Type info)
  | Action    info (Expr info)
  | WrongDecl      Error
  deriving (Show) via PP (Declaration info)

instance Stubbed (Declaration info) where stub = WrongDecl

data Binding info
  = Irrefutable  info (Pattern info) (Expr info)
  | Function     info Bool (Name info) [VarDecl info] (Type info) (Expr info)
  | Var          info (Name info) (Type info) (Expr info)
  | Const        info (Name info) (Type info) (Expr info)
  | WrongBinding      Error
  deriving (Show) via PP (Binding info)

instance Stubbed (Binding info) where stub = WrongBinding

data VarDecl info
  = Decl         info (Mutable info) (Name info) (Type info)
  | WrongVarDecl      Error
  deriving (Show) via PP (VarDecl info)

instance Stubbed (VarDecl info) where stub = WrongVarDecl

data Mutable info
  = Mutable      info
  | Immutable    info
  | WrongMutable      Error
  deriving (Show) via PP (Mutable info)


instance Stubbed (Mutable info) where stub = WrongMutable

data Type info
  = TArrow    info  (Type info) (Type info)
  | TRecord   info [TField info]
  | TVar      info  (Name info)
  | TSum      info [(Name info, [Type info])]
  | TProduct  info  [Type info]
  | TApply    info  (Name info) [Type info]
  | WrongType      Error
  deriving (Show) via PP (Type info)

instance Stubbed (Type info) where stub = WrongType

data TField info
  = TField info (Name info) (Type info)
  | WrongTField Error
  deriving (Show) via PP (TField info)

instance Stubbed (TField info) where stub = WrongTField

data Expr info
  = Let       info [Declaration info] (Expr info)
  | Apply     info (Expr info) [Expr info]
  | Constant  info (Constant info)
  | Ident     info (QualifiedName info)
  | BinOp     info (Expr info) Text (Expr info)
  | Record    info [Assignment info]
  | WrongExpr      Error
  deriving (Show) via PP (Expr info)

instance Stubbed (Expr info) where stub = WrongExpr

data Assignment info
  = Assignment info (Name info) (Expr info)
  | WrongAssignment Error
  deriving (Show) via PP (Assignment info)

instance Stubbed (Assignment info) where stub = WrongAssignment

data Constant info
  = Int     info Text
  | String  info Text
  | Float   info Text
  | Bytes   info Text
  | WrongConstant Error
  deriving (Show) via PP (Constant info)

instance Stubbed (Constant info) where stub = WrongConstant

data Pattern info
  = IsConstr     info (Name info) [Pattern info]
  | IsConstant   info (Constant info)
  | IsVar        info (Name info)
  | WrongPattern      Error
  deriving (Show) via PP (Pattern info)

instance Stubbed (Pattern info) where stub = WrongPattern

data QualifiedName info
  = QualifiedName
    { qnInfo   :: info
    , qnSource :: Name info
    , qnPath   :: [Path info]
    }
  | WrongQualifiedName Error
  deriving (Show) via PP (QualifiedName info)

instance Stubbed (QualifiedName info) where stub = WrongQualifiedName

data Path info
  = At info (Name info)
  | Ix info Text
  | WrongPath Error
  deriving (Show) via PP (Path info)

instance Stubbed (Path info) where stub = WrongPath

data Name info = Name
  { info    :: info
  , raw     :: Text
  }
  | WrongName Error

instance Stubbed (Name info) where stub = WrongName

instance Show (Name info) where
  show = \case
    Name _ raw  -> Text.unpack raw
    WrongName r -> "(Name? " ++ show r ++ ")"

instance Pretty (Contract i) where
  pp = \case
    Contract _ decls ->
      hang "(* contract *)" 2 do
        vcat $ map (($$ empty) . pp) decls

    WrongContract err ->
      pp err

instance Pretty (Declaration i) where
  pp = \case
    ValueDecl _ binding -> pp binding
    TypeDecl  _ n ty    -> hang ("type" <+> pp n <+> "=") 2 (pp ty)
    Action    _ e       -> pp e
    WrongDecl err       -> pp err

instance Pretty (Binding i) where
  pp = \case
    Irrefutable  _ pat expr -> error "irrefs in pascaligo?"
    Function     _ isRec name params ty body ->
      hang
        ( fsep
          [ if isRec then "recursive" else empty
          , "function"
          , pp name
          , tuple params
          , ":"
          , pp ty
          , "is"
          ]
        )
        2
        (pp body)
    Var   _ name ty value ->
      hang
        ("var" <+> pp name <+> ":" <+> pp ty <+> ":=")
        2
        (pp value)
    Const _ name ty body ->
      hang
        ("const" <+> pp name <+> ":" <+> pp ty <+> "=")
        2
        (pp body)
    WrongBinding err ->
      pp err

instance Pretty (VarDecl i) where
  pp = \case
    Decl _ mutability name ty -> fsep
      [ pp mutability
      , pp name
      , ":"
      , pp ty
      ]
    WrongVarDecl err ->
      pp err

instance Pretty (Mutable i) where
  pp = \case
    Mutable      _   -> "var"
    Immutable    _   -> "const"
    WrongMutable err -> pp err

instance Pretty (Type i) where
  pp = \case
    TArrow    _ dom codom -> parens (pp dom <+> "->" <+> pp codom)
    TRecord   _ fields    -> "record [" <> (vcat $ map pp fields) <> "]"
    TVar      _ name      -> pp name
    TSum      _ variants  -> vcat $ map ppCtor variants
    TProduct  _ elements  -> fsep $ punctuate " *" $ map pp elements
    TApply    _ f xs      -> pp f <> parens (fsep $ punctuate "," $ map pp xs)
    WrongType   err       -> pp err
    where
      ppField (name, ty) = pp name <> ": " <> pp ty <> ";"
      ppCtor  (ctor, fields) =
        "|" <+> pp ctor <+> parens (fsep $ punctuate "," $ map pp fields)

instance Pretty (Expr i) where
  pp = \case
    Let       _ decls body -> hang "block {" 2 (vcat $ map pp decls)
                           $$ hang "} with"  2 (pp body)
    Apply     _ f xs       -> pp f <> tuple xs
    Constant  _ constant   -> pp constant
    Ident     _ qname      -> pp qname
    BinOp     _ l o r      -> parens (pp l <+> pp o <+> pp r)
    Record    _ az         -> "record [" <> (fsep $ punctuate ";" $ map pp az) <> "]"
    WrongExpr   err        -> pp err

instance Pretty (Assignment i) where
  pp = \case
    Assignment      _ n e -> pp n <+> "=" <+> pp e
    WrongAssignment   err -> pp err

instance Pretty (Constant i) where
  pp = \case
    Int           _ c   -> pp c
    String        _ c   -> doubleQuotes (pp c)
    Float         _ c   -> pp c
    Bytes         _ c   -> pp c
    WrongConstant   err -> pp err

instance Pretty (QualifiedName i) where
  pp = \case
    QualifiedName _ src path -> pp src <> cat (map (("." <>) . pp) path)
    WrongQualifiedName err   -> pp err

instance Pretty (Pattern info) where
  pp = \case
    IsConstr     _ ctor args -> pp ctor <> tuple args
    IsConstant   _ c         -> pp c
    IsVar        _ name      -> pp name
    WrongPattern   err       -> pp err


instance Pretty (Name i) where
  pp = \case
    Name      _ raw -> pp raw
    WrongName err   -> pp err

instance Pretty (Path i) where
  pp = \case
    At _ n -> pp n
    Ix _ i -> pp i
    WrongPath err -> pp err

instance Pretty (TField i) where
  pp = \case
    TField _ n t -> hang (pp n <> ":") 2 (pp t)
    WrongTField err -> pp err

tuple :: Pretty p => [p] -> Doc
tuple xs = parens (fsep $ punctuate "," $ map pp xs)