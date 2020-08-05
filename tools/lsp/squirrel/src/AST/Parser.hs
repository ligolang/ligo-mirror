
{- | Parser for a contract.
-}

module AST.Parser
  -- (example, contract, sample)
  where

import Control.Arrow

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Sum (Element)

import AST.Types

import Duplo.Error
import Duplo.Tree
import Duplo.Pretty

import Range
import Product
import Parser
import ParseTree

import Debug.Trace

-- example = "../../../src/test/contracts/arithmetic.ligo"
-- example = "../../../src/test/contracts/address.ligo"
-- example = "../../../src/test/contracts/annotation.ligo"
-- example = "../../../src/test/contracts/amount.ligo"
-- example = "../../../src/test/contracts/attributes.ligo"
-- example = "../../../src/test/contracts/application.ligo"
-- example = "../../../src/test/contracts/assign.ligo"
-- example = "../../../src/test/contracts/big_map.ligo"
-- example = "../../../src/test/contracts/blockless.ligo"
-- example = "../../../src/test/contracts/bad_timestamp.ligo"
-- example = "../../../src/test/contracts/boolean_operators.ligo"
-- example = "../../../src/test/contracts/bitwise_arithmetic.ligo"
-- example = "../../../src/test/contracts/bad_type_operator.ligo"
-- example = "../../../src/test/contracts/blocks.ligo"
-- example = "../../../src/test/contracts/bytes_unpack.ligo"
-- example = "../../../src/test/contracts/balance_constant.ligo"
-- example = "../../../src/test/contracts/blockless.ligo"
-- example = "../../../src/test/contracts/bytes_arithmetic.ligo"
-- example = "../../../src/test/contracts/chain_id.ligo"
-- example = "../../../src/test/contracts/closure-3.ligo"
example = "../../../src/test/contracts/coase.ligo"

sample' :: FilePath -> IO (LIGO Info)
sample' f
  =   toParseTree (Path f)
  >>= runParserM . recognise
  >>= return . fst

source' :: FilePath -> IO ()
source' f
  =   toParseTree (Path f)
  >>= print . pp

sample :: IO ()
sample
  =   toParseTree (Path example)
  >>= runParserM . recognise
  >>= print . pp . fst

source :: IO ()
source
  =   toParseTree (Path example)
  >>= print . pp

recognise :: RawTree -> ParserM (LIGO Info)
recognise = descent (\_ -> error . show . pp) $ map usingScope
  [ -- Contract
    Descent do
      boilerplate \case
        "Start" -> RawContract <$> fields "declaration"
        _ -> fallthrough

    -- Expr
  , Descent do
      boilerplate \case
        "let_expr"          -> Let       <$> field  "locals"    <*> field "body"
        "fun_call"          -> Apply     <$> field  "f"         <*> field "arguments"
        "par_call"          -> Apply     <$> field  "f"         <*> field "arguments"
        "projection_call"   -> Apply     <$> field  "f"         <*> field "arguments"
        "Some_call"         -> Apply     <$> field  "constr"    <*> field "arguments"
        "constr_call"       -> Apply     <$> field  "constr"    <*> field "arguments"
        "arguments"         -> Tuple     <$> fields "argument"
        "unop"              -> UnOp      <$> field  "negate"    <*> field "arg"
        "binop"             -> BinOp     <$> field  "arg1"      <*> field "op"   <*> field "arg2"
        "block"             -> Seq       <$> fields "statement"
        "clause_block"      -> Seq       <$> fields "statement"
        "list_expr"         -> List      <$> fields "element"
        "annot_expr"        -> Annot     <$> field  "subject"   <*> field "type"
        "conditional"       -> If        <$> field  "selector"  <*> field "then" <*> field "else"
        "cond_expr"         -> If        <$> field  "selector"  <*> field "then" <*> field "else"
        "assignment"        -> Assign    <$> field  "LHS"       <*> field "RHS"
        "attr_decl"         -> Attrs     <$> fields "attribute"
        "record_expr"       -> Record    <$> fields "assignment"
        "big_map_injection" -> BigMap    <$> fields "binding"
        "map_remove"        -> MapRemove <$> field  "key"       <*> field "container"
        "tuple_expr"        -> Tuple     <$> fields "element"
        "skip"              -> return Skip
        "case_expr"         -> Case      <$> field  "subject"    <*> fields   "case"
        "case_instr"        -> Case      <$> field  "subject"    <*> fields   "case"
        "fun_expr"          -> Lambda    <$> field  "parameters" <*> field    "type"  <*> field "body"
        "for_cycle"         -> ForLoop   <$> field  "name"       <*> field    "begin" <*> field "end" <*> fieldOpt "step" <*> field "body"
        "for_box"           -> ForBox    <$> field  "key"        <*> fieldOpt "value" <*> field "kind"  <*> field "collection" <*> field "body"
        "while_loop"        -> WhileLoop <$> field  "breaker"    <*> field    "body"
        "map_injection"     -> Map       <$> fields "binding"
        "list_injection"    -> List      <$> fields "element"
        "set_expr"          -> Set       <$> fields "element"
        "map_patch"         -> MapPatch  <$> field  "container"  <*> fields "binding"
        "set_patch"         -> SetPatch  <$> field  "container"  <*> fields "key"
        "set_remove"        -> SetRemove <$> field  "key"        <*> field  "container"
        "map_remove"        -> SetRemove <$> field  "key"        <*> field  "container"
        "update_record"     -> RecordUpd <$> field  "record"     <*> fields "assignment"
        _                   -> fallthrough

    -- Pattern
  , Descent do
      boilerplate \case
        "user_constr_pattern" -> IsConstr <$> field  "constr" <*> fieldOpt "arguments"
        "tuple_pattern"       -> IsTuple  <$> fields "element"
        "nil"                 -> return $ IsList []
        "list_pattern"        -> IsList   <$> fields "element"
        "cons_pattern"        -> IsCons   <$> field  "head"   <*> field "tail"
        _                     -> fallthrough

    -- Alt
  , Descent do
      boilerplate \case
        "case_clause_expr"  -> Alt <$> field "pattern" <*> field  "body"
        "case_clause_instr" -> Alt <$> field "pattern" <*> field  "body"
        _                   -> fallthrough

    -- FieldAssignment
  , Descent do
      boilerplate \case
        "field_assignment"      -> FieldAssignment <$> field "name" <*> field "_rhs"
        "field_path_assignment" -> FieldAssignment <$> field "lhs"  <*> field "_rhs"
        _                  -> fallthrough

    -- MapBinding
  , Descent do
      boilerplate \case
        "binding" -> MapBinding <$> field "key" <*> field "value"
        _         -> fallthrough

  , Descent do
      boilerplate' \case
        ("negate",     op) -> return $ Op op
        ("adder",      op) -> return $ Op op
        ("multiplier", op) -> return $ Op op
        ("comparison", op) -> return $ Op op
        ("^",          _)  -> return $ Op "^"
        ("#",          _)  -> return $ Op "#"
        _                  -> fallthrough

  , Descent do
      boilerplate \case
        "data_projection" -> QualifiedName <$> field "struct"    <*> fields "index"
        "map_lookup"      -> QualifiedName <$> field "container" <*> fields "index"
        "module_field"    -> QualifiedName <$> field "module"    <*> fields "method"
        _                 -> fallthrough

    -- Literal
  , Descent do
      boilerplate' \case
        ("Int",    i) -> return $ Int i
        ("Nat",    i) -> return $ Nat i
        ("Bytes",  i) -> return $ Bytes i
        ("String", i) -> return $ String i
        ("Tez",    i) -> return $ Tez i
        _             -> fallthrough

    -- Declaration
  , Descent do
      boilerplate \case
        "fun_decl"   -> Function <$> (isJust <$> fieldOpt "recursive") <*> field "name" <*> field "parameters" <*> field "type" <*> field "body"
        "const_decl" -> Const    <$>             field    "name"       <*> field "type" <*> field "value"
        "var_decl"   -> Var      <$>             field    "name"       <*> field "type" <*> field "value"
        "type_decl"  -> TypeDecl <$>             field    "typeName"   <*> field "typeValue"
        "include"    -> Include  <$>             field    "filename"
        _            -> fallthrough

    -- Parameters
  , Descent do
      boilerplate \case
        "parameters" -> Parameters <$> fields "parameter"
        _            -> fallthrough

    -- VarDecl
  , Descent do
      boilerplate \case
        "param_decl" -> Decl <$> field "access" <*> field "name" <*> field "type"
        _            -> fallthrough

    -- Mutable
  , Descent do
      boilerplate \case
        "const" -> return Immutable
        "var"   -> return Mutable
        _       -> fallthrough

    -- Name
  , Descent do
      boilerplate' \case
        ("Name", n) -> return $ Name n
        ("and", _)  -> return $ Name "and"
        ("or", _)   -> return $ Name "or"
        _           -> fallthrough

    -- Type
  , Descent do
      boilerplate \case
        "fun_type"         -> TArrow   <$> field  "domain"     <*> field "codomain"
        "cartesian"        -> TProduct <$> fields "element"
        "invokeBinary"     -> TApply   <$> field  "typeConstr" <*> field "arguments"
        "invokeUnary"      -> TApply   <$> field  "typeConstr" <*> field "arguments"
        "type_tuple"       -> TTuple   <$> fields "element"
        "record_type"      -> TRecord  <$> fields "field"
        "sum_type"         -> TSum     <$> fields "variant"
        "michelsonTypeOr"  -> TOr      <$> field "left_type" <*> field "left_type_name" <*> field "right_type" <*> field "right_type_name"
        "michelsonTypeAnd" -> TAnd     <$> field "left_type" <*> field "left_type_name" <*> field "right_type" <*> field "right_type_name"
        _                 -> fallthrough

    -- Variant
  , Descent do
      boilerplate \case
        "variant" -> Variant <$> field "constructor" <*> fieldOpt "arguments"
        _         -> fallthrough

    -- TField
  , Descent do
      boilerplate \case
        "field_decl" -> TField <$> field "fieldName" <*> field "fieldType"
        _            -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' \case
        ("TypeName", name) -> return $ TypeName name
        ("list",     _)    -> return $ TypeName "list"
        ("big_map",  _)    -> return $ TypeName "big_map"
        ("map",      _)    -> return $ TypeName "map"
        ("set",      _)    -> return $ TypeName "set"
        ("option",   _)    -> return $ TypeName "option"
        ("contract", _)    -> return $ TypeName "contract"
        _                  -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' \case
        ("Name_Capital", name) -> return $ Ctor name
        ("Some", _)            -> return $ Ctor "Some"
        ("Some_pattern", _)    -> return $ Ctor "Some"
        ("None", _)            -> return $ Ctor "None"
        ("True", _)            -> return $ Ctor "True"
        ("False", _)           -> return $ Ctor "False"
        ("Unit", _)            -> return $ Ctor "Unit"
        ("constr", n)          -> return $ Ctor n
        _                      -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' \case
        ("FieldName", name) -> return $ FieldName name
        _                   -> fallthrough

    -- Err
  , Descent do
      \(r :> _, ParseTree _ _ text) -> do
        withComments do
          return (r :> N :> Nil, Err text)

  , Descent do
      \case
        (r :> _, ParseTree "ERROR" _ text) -> do
          return ([] :> r :> Y :> Nil, Err text)

        _ -> fallthrough
  ]
