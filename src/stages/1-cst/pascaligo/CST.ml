(* Concrete Syntax Tree (CST) for LIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30-40-42"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

type 'a reg = 'a Region.reg

open Utils

(* Lexemes *)

type lexeme = string

(* Keywords of PascaLIGO *)

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type keyword        = Region.t

type kwd_and       = Region.t
type kwd_begin     = Region.t
type kwd_big_map   = Region.t
type kwd_block     = Region.t
type kwd_case      = Region.t
type kwd_const     = Region.t
type kwd_contains  = Region.t
type kwd_down      = Region.t
type kwd_else      = Region.t
type kwd_end       = Region.t
type kwd_for       = Region.t
type kwd_from      = Region.t
type kwd_function  = Region.t
type kwd_if        = Region.t
type kwd_in        = Region.t
type kwd_is        = Region.t
type kwd_list      = Region.t
type kwd_map       = Region.t
type kwd_mod       = Region.t
type kwd_module    = Region.t
type kwd_nil       = Region.t
type kwd_not       = Region.t
type kwd_of        = Region.t
type kwd_or        = Region.t
type kwd_patch     = Region.t
type kwd_record    = Region.t
type kwd_recursive = Region.t
type kwd_remove    = Region.t
type kwd_set       = Region.t
type kwd_skip      = Region.t
type kwd_step      = Region.t
type kwd_then      = Region.t
type kwd_to        = Region.t
type kwd_type      = Region.t
type kwd_var       = Region.t
type kwd_while     = Region.t
type kwd_with      = Region.t

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = Region.t  (* "->"  *)
type assign   = Region.t  (* ":="  *)
type caret    = Region.t  (* "^"   *)
type colon    = Region.t  (* ":"   *)
type comma    = Region.t  (* ","   *)
type cons     = Region.t  (* "#"   *)
type dot      = Region.t  (* "."   *)
type equal    = Region.t  (* "="   *)
type geq      = Region.t  (* ">="  *)
type gt       = Region.t  (* ">"   *)
type lbrace   = Region.t  (* "{"   *)
type lbracket = Region.t  (* "["   *)
type leq      = Region.t  (* "<="  *)
type lpar     = Region.t  (* "("   *)
type lt       = Region.t  (* "<"   *)
type minus    = Region.t  (* "-"   *)
type neq      = Region.t  (* "=/=" *)
type plus     = Region.t  (* "+"   *)
type rbrace   = Region.t  (* "}"   *)
type rbracket = Region.t  (* "]"   *)
type rpar     = Region.t  (* ")"   *)
type semi     = Region.t  (* ";"   *)
type slash    = Region.t  (* "/"   *)
type times    = Region.t  (* "*"   *)
type vbar     = Region.t  (* "|"   *)

(* End-of-File *)

type eof = Region.t

(* Literals *)

type variable    = string reg
type module_name = string reg
type fun_name    = string reg
type type_name   = string reg
type type_var    = string reg
type type_ctor   = string reg
type field_name  = string reg
type ctor        = string reg
type attribute   = string reg
type language    = string reg

(* Parentheses *)

type 'a par = {
  lpar   : lpar;
  inside : 'a;
  rpar   : rpar
}

(* Brackets *)

type 'a brackets = {
  lbracket : lbracket;
  inside   : 'a;
  rbracket : rbracket
}

(* Collections *)

type iterable = [
  `List   of kwd_list
| `Map    of kwd_map
| `BigMap of kwd_big_map
| `Set    of kwd_set
]

type compound_type = [
  iterable
| `Record of kwd_record
]

(* The Concrete Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and cst = t

(* ATTRIBUTES *)

and attributes = attribute list

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Const     of const_decl   reg
| D_Directive of Directive.t
| D_Fun       of fun_decl     reg
| D_Module    of module_decl  reg
| D_ModAlias  of module_alias reg
| D_Type      of type_decl    reg

(* Constant declaration *)

and const_decl = {
  kwd_const  : kwd_const;
  pattern    : pattern;
  const_type : type_annotation option;
  equal      : equal;
  init       : expr;
  terminator : semi option;
  attributes : attributes
}

and type_annotation = colon * type_expr

(* Function declaration *)

and fun_decl = {
  kwd_recursive : kwd_recursive option;
  kwd_function  : kwd_function;
  fun_name      : variable;
  param         : parameters;
  ret_type      : type_annotation option;
  kwd_is        : kwd_is;
  return        : expr;
  terminator    : semi option;
  attributes    : attributes
}

and parameters = (param_decl reg, semi) nsepseq par reg

and param_decl = {
  param_kind : [`Var of kwd_var | `Const of kwd_const];
  var        : var_pattern reg;
  param_type : type_annotation option
}

and var_pattern = {
  variable   : variable;
  attributes : attributes
}

(* Module declaration (structures) *)

and module_decl = {
  kwd_module   : kwd_module;
  name         : module_name;
  kwd_is       : kwd_is;
  enclosing    : block_enclosing;
  declarations : declaration nseq;
  terminator   : semi option
}

(* Declaration of module alias *)

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  kwd_is     : kwd_is;
  mod_path   : (module_name, dot) nsepseq;
  terminator : semi option
}

(* Type declaration *)

and type_decl = {
  kwd_type   : kwd_type;
  name       : type_name;
  params     : type_vars option;
  kwd_is     : kwd_is;
  type_expr  : type_expr;
  terminator : semi option
}

and type_vars = (type_var, comma) nsepseq par reg

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_Cart    of cartesian
| T_Ctor    of type_ctor_app reg
| T_Fun     of (type_expr * arrow * type_expr) reg
| T_Int     of (lexeme * Z.t) reg
| T_ModPath of type_expr module_path reg
| T_Par     of type_expr par reg
| T_Record  of field_decl reg compound reg
| T_String  of lexeme reg
| T_Sum     of sum_type reg
| T_Var     of variable

(* Application of type constructors *)

and type_ctor_app = type_ctor module_path reg * type_tuple

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

and type_tuple = (type_expr, comma) nsepseq par reg

(* Cartesian types *)

and cartesian = (type_expr, times) nsepseq reg

(* Record types *)

and field_decl = {
  field_name : field_name;
  field_type : type_annotation option;
  attributes : attributes
}

(* Injections *)

and 'a compound = {
  kind       : compound_type;
  enclosing  : enclosing;
  elements   : ('a, semi) sepseq;
  terminator : semi option;
  attributes : attributes
}

and enclosing =
  Brackets of lbracket * rbracket
| End      of kwd_end

(* Sum types *)

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attributes
}

and variant = {
  ctor       : ctor;
  args       : (kwd_of * type_expr) option;
  attributes : attributes
}

(* STATEMENTS *)

and statement =
  S_Instr   of instruction
| S_Decl    of declaration
| S_VarDecl of var_decl reg

and statements = (statement, semi) nsepseq

(* Variable declaration (not valid at the top-level) *)

and var_decl = {
  kwd_var    : kwd_var;
  pattern    : pattern;
  var_type   : type_annotation option;
  assign     : assign;
  init       : expr;
  terminator : semi option;
  attributes : attributes
}

(* INSTRUCTIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and instruction =
  I_Assign of assignment reg
| I_Call   of call
| I_Case   of test_clause case reg
| I_Cond   of (test_clause, test_clause) conditional reg
| I_For    of for_int reg
| I_ForIn  of for_in reg
| I_Patch  of patch reg
| I_Remove of removal reg
| I_Skip   of kwd_skip
| I_While  of while_loop reg

(* Assignment *)

and assignment = {
  lhs    : expr;
  assign : assign;
  rhs    : expr
}

(* Procedure call *)

and call = (expr * arguments) reg

and arguments = tuple_expr

and tuple_expr = (expr, comma) nsepseq par reg

(* Case *)

and 'a case = {
  kwd_case  : kwd_case;
  expr      : expr;
  kwd_of    : kwd_of;
  enclosing : enclosing;
  lead_vbar : vbar option;
  cases     : ('a case_clause reg, vbar) nsepseq reg
}

and 'a case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : 'a
}

and test_clause =
  ClauseInstr of instruction
| ClauseBlock of block reg

(* Blocks *)

and block = {
  enclosing  : block_enclosing;
  statements : statements;
  terminator : semi option
}

and block_enclosing =
  Braces   of kwd_block option * lbrace * rbrace
| BeginEnd of kwd_begin * kwd_end

(* Conditionals *)

and ('if_so, 'if_not) conditional = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  if_so    : 'if_so;
  if_not   : (kwd_else * 'if_not) option
}

(* Interation over integer intervals *)

and for_int = {
  kwd_for : kwd_for;
  index   : variable;
  assign  : assign;
  init    : expr;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option;
  block   : block reg
}

(* Iteration over collections *)

and for_in = {
  kwd_for  : kwd_for;
  var      : variable;
  bind_to  : (arrow * variable) option;
  kwd_in   : kwd_in;
  iterated : iterable;
  expr     : expr;
  block    : block reg
}

(* Patches *)

and patch = {
  kwd_patch  : kwd_patch;
  collection : expr;
  kwd_with   : kwd_with;
  delta      : expr
}

and binding = {
  source : expr;
  arrow  : arrow;
  image  : expr
}

(* Removal from collections *)

and removal = {
  kwd_remove : kwd_remove;
  item       : expr;
  kwd_from   : kwd_from;
  collection : expr
}

(* General loop *)

and while_loop = {
  kwd_while : kwd_while;
  cond      : expr;
  block     : block reg
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and pattern =
  P_Bytes  of (lexeme * Hex.t) reg
| P_Cons   of (pattern, cons) nsepseq reg
| P_Ctor   of data_ctor_pattern reg
| P_Int    of (lexeme * Z.t) reg
| P_List   of pattern compound reg
| P_Nat    of (lexeme * Z.t) reg
| P_Nil    of kwd_nil
| P_Par    of pattern par reg
| P_Record of record_pattern reg
| P_String of lexeme reg
| P_Tuple  of tuple_pattern
| P_Typed  of typed_pattern reg
| P_Var    of var_pattern reg

(* Record pattern *)

and record_pattern = (var_pattern, pattern) field reg compound

(* Pattern for data constructor application *)

and data_ctor_pattern = ctor module_path reg * tuple_pattern option

and tuple_pattern = (pattern, comma) nsepseq par reg

(* Typed pattern *)

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annotation
}

(* Record fields *)

and ('lhs, 'rhs) field =
  Punned   of 'lhs punned
| Complete of ('lhs, 'rhs) full_field

and 'lhs punned = {
  pun        : 'lhs;
  attributes : attributes
}

and ('lhs, 'rhs) full_field = {
  field_lhs  : 'lhs;
  assign     : equal;
  field_rhs  : 'rhs;
  attributes : attributes
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add       of plus bin_op reg               (* "+"   *)
| E_And       of kwd_and bin_op reg            (* "and" *)
| E_BigMap    of binding reg compound reg
| E_Block     of block_with reg
| E_Bytes     of (lexeme * Hex.t) reg
| E_Call      of call
| E_Case      of expr case reg
| E_Cat       of caret bin_op reg              (* "^"   *)
| E_CodeInj   of code_inj reg
| E_Equal     of equal bin_op reg              (* "="   *)
| E_Cond      of (expr, expr) conditional reg
| E_Cons      of cons bin_op reg
| E_Ctor      of data_ctor_app reg
| E_Div       of slash bin_op reg              (* "/"   *)
| E_Fun       of fun_expr reg
| E_Geq       of geq bin_op reg                (* ">="  *)
| E_Gt        of gt bin_op reg                 (* ">"   *)
| E_Int       of (lexeme * Z.t) reg
| E_Leq       of leq bin_op reg                (* "<="  *)
| E_List      of expr compound reg
| E_Lt        of lt bin_op reg                 (* "<"   *)
| E_Map       of binding reg compound reg
| E_MapLookup of map_lookup reg
| E_Mod       of kwd_mod bin_op reg            (* "mod" *)
| E_Mult      of times bin_op reg              (* "*"   *)
| E_Mutez     of (lexeme * Z.t) reg
| E_Nat       of (lexeme * Z.t) reg
| E_Neg       of minus un_op reg               (* "-a"  *)
| E_Nil       of kwd_nil
| E_Neq       of neq bin_op reg                (* "=/=" *)
| E_Not       of kwd_not un_op reg             (* "not" *)
| E_Or        of kwd_or bin_op reg             (* "or"  *)
| E_Par       of expr par reg
| E_Record    of record_expr reg
| E_Set       of expr compound reg
| E_SetMem    of set_membership reg
| E_String    of lexeme reg
| E_Sub       of minus bin_op reg              (* "a-b" *)
| E_Tuple     of tuple_expr
| E_Typed     of typed_expr par reg
| E_Update    of update reg
| E_Verbatim  of lexeme reg
| E_ModPath   of expr module_path reg
| E_Var       of lexeme reg
| E_Proj      of projection reg

(* Binary and unary arithmetic operators *)

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

(* Block as expression *)

and block_with = {
  block    : block reg;
  kwd_with : kwd_with;
  expr     : expr
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language reg;
  code     : expr;
  rbracket : rbracket
}

(* Application of a data constructor *)

and data_ctor_app = ctor module_path reg * arguments option

(* Functional expression *)

and fun_expr = {
  kwd_function : kwd_function;
  param        : parameters;
  ret_type     : type_annotation option;
  kwd_is       : kwd_is;
  return       : expr;
  attributes   : attributes
}

(* Map lookup *)

and map_lookup = {
  map   : expr;
  index : expr brackets reg
}

and path =
  LocalPath of local_path
| InModule  of local_path module_path reg

and local_path =
  Name     of variable
| InRecord of projection reg

and projection = {
  record     : expr;
  selector   : dot;
  field_path : (selection, dot) nsepseq
}

and selection =
  FieldName of field_name
| Component of (lexeme * Z.t) reg

(* Record expression *)

and record_expr = (path, expr) field reg compound

(* Set membership *)

and set_membership = {
  set          : expr;
  kwd_contains : kwd_contains;
  element      : expr
}

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Updates *)

and update = {
  structure : expr;
  kwd_with  : kwd_with;
  update    : expr
}

(* PROJECTING REGIONS *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last to_region tl)

let nsepseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last (to_region <@ snd) tl)

let sepseq_to_region to_region = function
      None -> Region.ghost
| Some seq -> nsepseq_to_region to_region seq

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let type_expr_to_region = function
  T_Ctor    {region; _}
| T_Fun     {region; _}
| T_Int     {region; _}
| T_ModPath {region; _}
| T_Par     {region; _}
| T_Cart    {region; _}
| T_Record  {region; _}
| T_String  {region; _}
| T_Sum     {region; _}
| T_Var     {region; _}
  -> region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let expr_to_region = function
  E_Add       {region; _}
| E_And       {region; _}
| E_Typed     {region; _}
| E_BigMap    {region; _}
| E_Block     {region; _}
| E_Bytes     {region; _}
| E_Call      {region; _}
| E_Case      {region; _}
| E_Cat       {region; _}
| E_CodeInj   {region; _}
| E_Equal     {region; _}
| E_Cond      {region; _}
| E_Cons      {region; _}
| E_Ctor      {region; _}
| E_Div       {region; _}
| E_Fun       {region; _}
| E_Geq       {region; _}
| E_Gt        {region; _}
| E_Int       {region; _}
| E_Leq       {region; _}
| E_List      {region; _}
| E_Lt        {region; _}
| E_Map       {region; _}
| E_MapLookup {region; _}
| E_Mod       {region; _}
| E_ModPath   {region; _}
| E_Mult      {region; _}
| E_Mutez     {region; _}
| E_Nat       {region; _}
| E_Neg       {region; _}
| E_Nil        region
| E_Neq       {region; _}
| E_Not       {region; _}
| E_Or        {region; _}
| E_Par       {region; _}
| E_Proj      {region; _}
| E_Record    {region; _}
| E_Set       {region; _}
| E_SetMem    {region; _}
| E_String    {region; _}
| E_Sub       {region; _}
| E_Tuple     {region; _}
| E_Update    {region; _}
| E_Var       {region; _}
| E_Verbatim  {region; _}
  -> region

and tuple_expr_to_region x = x.Region.region

and typed_expr_to_region x = x.Region.region

and record_expr_to_region x = x.Region.region

let local_path_to_region = function
  Name var  -> var.region
| InRecord path -> path.region

let path_to_region = function
  LocalPath p -> local_path_to_region p
| InModule  p -> p.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let instr_to_region = function
  I_Assign {region; _}
| I_Call   {region; _}
| I_Case   {region; _}
| I_Cond   {region; _}
| I_For    {region; _}
| I_ForIn  {region; _}
| I_Patch  {region; _}
| I_Remove {region; _}
| I_Skip    region
| I_While  {region; _}
  -> region

let test_clause_to_region = function
  ClauseInstr instr -> instr_to_region instr
| ClauseBlock block -> block.Region.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let pattern_to_region = function
  P_Bytes   {region; _}
| P_Cons    {region; _}
| P_Ctor    {region; _}
| P_Int     {region; _}
| P_List    {region; _}
| P_Nat     {region; _}
| P_Nil      region
| P_Par     {region; _}
| P_Record  {region; _}
| P_String  {region; _}
| P_Tuple   {region; _}
| P_Typed   {region; _}
| P_Var     {region; _}
  -> region

let declaration_to_region = function
  D_Const    {region; _}
| D_Fun      {region; _}
| D_Module   {region; _}
| D_ModAlias {region; _}
| D_Type     {region; _} -> region
| D_Directive d -> Directive.to_region d

let selection_to_region = function
  FieldName {region; _}
| Component {region; _} -> region
