(* Concrete Syntax Tree (CST) for PascaLIGO *)

(* Disabled warnings *)

[@@@warning "-30"] (* multiply-defined record labels *)

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

(* Local dependencies *)

module Token = Lexing_pascaligo.Token
module Wrap  = Lexing_shared.Wrap
module Attr  = Lexing_shared.Attr

(* Utilities *)

type 'a reg = 'a Region.reg
type 'payload wrap = 'payload Wrap.t

open Utils

type lexeme = string

(* Keywords of PascaLIGO *)

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type kwd_and       = lexeme wrap
type kwd_begin     = lexeme wrap
type kwd_big_map   = lexeme wrap
type kwd_block     = lexeme wrap
type kwd_case      = lexeme wrap
type kwd_const     = lexeme wrap
type kwd_contains  = lexeme wrap
type kwd_down      = lexeme wrap
type kwd_else      = lexeme wrap
type kwd_end       = lexeme wrap
type kwd_for       = lexeme wrap
type kwd_from      = lexeme wrap
type kwd_function  = lexeme wrap
type kwd_if        = lexeme wrap
type kwd_in        = lexeme wrap
type kwd_is        = lexeme wrap
type kwd_list      = lexeme wrap
type kwd_map       = lexeme wrap
type kwd_mod       = lexeme wrap
type kwd_module    = lexeme wrap
type kwd_nil       = lexeme wrap
type kwd_not       = lexeme wrap
type kwd_of        = lexeme wrap
type kwd_or        = lexeme wrap
type kwd_patch     = lexeme wrap
type kwd_record    = lexeme wrap
type kwd_recursive = lexeme wrap
type kwd_remove    = lexeme wrap
type kwd_set       = lexeme wrap
type kwd_skip      = lexeme wrap
type kwd_step      = lexeme wrap
type kwd_then      = lexeme wrap
type kwd_to        = lexeme wrap
type kwd_type      = lexeme wrap
type kwd_var       = lexeme wrap
type kwd_while     = lexeme wrap
type kwd_with      = lexeme wrap

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = lexeme wrap  (* ->  *)
type assign   = lexeme wrap  (* :=  *)
type caret    = lexeme wrap  (* ^   *)
type colon    = lexeme wrap  (* :   *)
type comma    = lexeme wrap  (* ,   *)
type sharp    = lexeme wrap  (* #   *)
type dot      = lexeme wrap  (* .   *)
type equal    = lexeme wrap  (* =   *)
type geq      = lexeme wrap  (* >=  *)
type gt       = lexeme wrap  (* >   *)
type lbrace   = lexeme wrap  (* {   *)
type lbracket = lexeme wrap  (* [   *)
type leq      = lexeme wrap  (* <=  *)
type lpar     = lexeme wrap  (* (   *)
type lt       = lexeme wrap  (* <   *)
type minus    = lexeme wrap  (* -   *)
type neq      = lexeme wrap  (* =/= *)
type plus     = lexeme wrap  (* +   *)
type rbrace   = lexeme wrap  (* }   *)
type rbracket = lexeme wrap  (* ]   *)
type rpar     = lexeme wrap  (* )   *)
type semi     = lexeme wrap  (* ;   *)
type slash    = lexeme wrap  (* /   *)
type times    = lexeme wrap  (* *   *)
type vbar     = lexeme wrap  (* |   *)

(* End-of-File *)

type eof = lexeme wrap

(* Literals *)

type variable    = lexeme wrap
type module_name = lexeme wrap
type field_name  = lexeme wrap
type ctor        = lexeme wrap

type language    = lexeme reg   (* Not [wrap] *)

type attribute   = Attr.t
type attributes  = Attr.attributes

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

(* CONCRETE SYNTAX TREE (CST) *)

type t = {
  decl : declarations;
  eof  : eof
}

and cst = t

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declarations = declaration nseq

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
  parameters    : parameters;
  ret_type      : type_annotation option;
  kwd_is        : kwd_is;
  return        : expr;
  terminator    : semi option;
  attributes    : attributes
}

and parameters = (param_decl reg, semi) nsepseq par reg

and param_decl = {
  param_kind : [`Var of kwd_var | `Const of kwd_const];
  var        : variable;
  param_type : type_annotation option
}

(* Module declaration (structures) *)

and module_decl = {
  kwd_module   : kwd_module;
  name         : module_name;
  kwd_is       : kwd_is;
  enclosing    : block_enclosing;
  declarations : declarations;
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
  name       : variable;
  params     : type_vars option;
  kwd_is     : kwd_is;
  type_expr  : type_expr;
  terminator : semi option
}

and type_vars = (variable, comma) nsepseq par reg

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_App     of (type_expr * type_tuple) reg        (* M.t (x,y,z)     *)
| T_Cart    of cartesian                           (* x * (y * z)     *)
| T_Fun     of (type_expr * arrow * type_expr) reg (* x -> y          *)
| T_Int     of (lexeme * Z.t) wrap                 (* 42              *)
| T_ModPath of type_expr module_path reg           (* A.B.(x * y)     *)
| T_Par     of type_expr par reg                   (* (t)             *)
| T_Record  of field_decl reg compound reg (* record [a; [@a1] b : t] *)
| T_String  of lexeme wrap                         (*    "x"          *)
| T_Sum     of sum_type reg               (* [@a2] | [@aa] A | B of t *)
| T_Var     of variable                            (*  x              *)

(* Application of type constructors *)

and type_tuple = (type_expr, comma) nsepseq par reg

(* Cartesian type *)

and cartesian = (type_expr * times * (type_expr,times) nsepseq) reg

(* Module paths *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* Compound constructs (lists, sets, records, maps) *)

and 'a compound = {
  kind       : lexeme wrap;
  enclosing  : enclosing;
  elements   : ('a, semi) sepseq;
  terminator : semi option;
  attributes : attributes
}

and enclosing =
  Brackets of lbracket * rbracket
| End      of kwd_end

(* Record types *)

and field_decl = {
  field_name : field_name;
  field_type : type_annotation option; (* Type punning if [None] *)
  attributes : attributes
}

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

and statements = (statement, semi) nsepseq

and statement =
  S_Instr   of instruction
| S_Decl    of declaration
| S_VarDecl of var_decl reg

(* Variable declaration (invalid at the top-level) *)

and var_decl = {
  kwd_var    : kwd_var;
  pattern    : pattern;
  var_type   : type_annotation option;
  assign     : assign;                 (* := *)
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
| I_Cond   of test_clause conditional reg
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
  cases     : ('a case_clause reg, vbar) nsepseq
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

(* General conditionals *)

and 'branch conditional = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  if_so    : 'branch;
  if_not   : (kwd_else * 'branch) option
}

(* Interation over integer intervals *)

and for_int = {
  kwd_for : kwd_for;
  index   : variable;
  assign  : assign;
  init    : expr;
  kwd_to  : kwd_to;
  bound   : expr;
  step    : (kwd_step * expr) option; (* [1] if [None] *)
  block   : block reg
}

(* Iteration over collections *)

and for_in = {
  kwd_for    : kwd_for;
  var        : variable;
  bind_to    : (arrow * variable) option; (* A map if not [None]. *)
  kwd_in     : kwd_in;
  kind       : kind option; (* For backward compatibility *)
  collection : expr;
  block      : block reg
}

and kind =
  Set  of kwd_set
| List of kwd_list
| Map  of kwd_map

(* Patches for maps, records, and sets. *)

and patch = {
  kwd_patch  : kwd_patch;
  collection : expr;
  kwd_with   : kwd_with;
  patch      : expr
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
  P_App     of (pattern * tuple_pattern option) reg
| P_Bytes   of (lexeme * Hex.t) wrap
| P_Cons    of (pattern * sharp * pattern) reg
| P_Ctor    of ctor
| P_Int     of (lexeme * Z.t) wrap
| P_List    of pattern compound reg
| P_ModPath of pattern module_path reg
| P_Mutez   of (lexeme * Int64.t) wrap
| P_Nat     of (lexeme * Z.t) wrap
| P_Nil     of kwd_nil
| P_Par     of pattern par reg
| P_Record  of record_pattern reg
| P_String  of lexeme wrap
| P_Tuple   of tuple_pattern
| P_Typed   of typed_pattern reg
| P_Var     of variable

(* Tuple pattern *)

and tuple_pattern = (pattern, comma) nsepseq par reg

(* Record pattern *)

and record_pattern = field_pattern reg compound

and field_pattern = (field_name, pattern) field

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

(* Typed pattern *)

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annotation
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add       of plus bin_op reg               (* x + y           *)
| E_App       of (expr * arguments option) reg (* M.t (x,y)       *)
| E_And       of kwd_and bin_op reg            (* x and y         *)
| E_BigMap    of binding reg compound reg
| E_Block     of block_with reg
| E_Bytes     of (lexeme * Hex.t) wrap         (* 0xFFFA          *)
| E_Call      of call                          (* M.f (x,y)       *)
| E_Case      of expr case reg
| E_Cat       of caret bin_op reg              (* "Hello" ^ world *)
| E_CodeInj   of code_inj reg
| E_Ctor      of ctor                          (* Empty           *)
| E_Equal     of equal bin_op reg              (* x = y           *)
| E_Cond      of expr conditional reg
| E_Cons      of sharp bin_op reg              (* head :: tail    *)
| E_Div       of slash bin_op reg              (* x / y           *)
| E_Fun       of fun_expr reg                  (* fun x -> x      *)
| E_Geq       of geq bin_op reg                (* x >= y          *)
| E_Gt        of gt bin_op reg                 (* x > y           *)
| E_Int       of (lexeme * Z.t) wrap           (* 42              *)
| E_Leq       of leq bin_op reg                (* x <= y          *)
| E_List      of expr compound reg             (* list [4;5]      *)
| E_Lt        of lt bin_op reg                 (* x < y           *)
| E_Map       of binding reg compound reg      (* map [3 -> "x"]  *)
| E_MapLookup of map_lookup reg                (* M.m [i]         *)
| E_Mod       of kwd_mod bin_op reg            (* x mod n         *)
| E_Mult      of times bin_op reg              (* x * y           *)
| E_Mutez     of (lexeme * Int64.t) wrap       (* 5mutez          *)
| E_Nat       of (lexeme * Z.t) wrap           (* 4n              *)
| E_Neg       of minus un_op reg               (* -a              *)
| E_Nil       of kwd_nil                       (* nil             *)
| E_Neq       of neq bin_op reg                (* x =/= y         *)
| E_Not       of kwd_not un_op reg             (* not x           *)
| E_Or        of kwd_or bin_op reg             (* x or y          *)
| E_Par       of expr par reg                  (* (x - M.y)       *)
| E_Record    of record_expr
| E_Set       of expr compound reg             (* set [x; 1]      *)
| E_SetMem    of set_membership reg            (* x contains y    *)
| E_String    of lexeme wrap                   (* "string"        *)
| E_Sub       of minus bin_op reg              (* a - b           *)
| E_Tuple     of tuple_expr                    (* (1, 2)          *)
| E_Typed     of typed_expr par reg            (* (1 : int)       *)
| E_Update    of update reg                    (* x with y        *)
| E_Verbatim  of lexeme wrap                   (* {|foo|}         *)
| E_ModPath   of expr module_path reg          (* M.N.x           *)
| E_Var       of lexeme wrap                   (* x               *)
| E_Proj      of projection reg                (* e.x.1           *)

(* Map binding *)

and binding = {
  key   : expr;
  arrow : arrow;
  value : expr
}

(* Block as expression *)

and block_with = {
  block    : block reg;
  kwd_with : kwd_with;
  expr     : expr
}

(* Projection *)

and projection = {
  record_or_tuple : expr;
  selector        : dot;
  field_path      : (selection, dot) nsepseq
}

and selection =
  FieldName of field_name
| Component of (lexeme * Z.t) wrap

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

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
 language : language reg;
 code     : expr;
 rbracket : rbracket
}

(* Functional expression *)

and fun_expr = {
  kwd_function : kwd_function;
  parameters   : parameters;
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

(* Record expression *)

and record_expr = (expr, expr) field reg compound reg

(* Set membership *)

and set_membership = {
  set          : expr;
  kwd_contains : kwd_contains;
  element      : expr
}

(* Typed expression *)

and typed_expr = expr * type_annotation

(* Functional updates *)

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
  T_App     {region; _}
| T_Cart    {region; _}
| T_Fun     {region; _} -> region
| T_Int     t -> t#region
| T_ModPath {region; _}
| T_Par     {region; _}
| T_Record  {region; _} -> region
| T_String  t -> t#region
| T_Sum     {region; _} -> region
| T_Var t -> t#region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let expr_to_region = function
  E_Add       {region; _}
| E_And       {region; _}
| E_BigMap    {region; _}
| E_Block     {region; _} -> region
| E_Bytes     t -> t#region
| E_Call      {region; _}
| E_Case      {region; _}
| E_Cat       {region; _}
| E_CodeInj   {region; _} -> region
| E_Ctor      t -> t#region
| E_Equal     {region; _}
| E_Cond      {region; _}
| E_Cons      {region; _}
| E_App       {region; _}
| E_Div       {region; _}
| E_Fun       {region; _}
| E_Geq       {region; _}
| E_Gt        {region; _} -> region
| E_Int       t -> t#region
| E_Leq       {region; _}
| E_List      {region; _}
| E_Lt        {region; _}
| E_Map       {region; _}
| E_MapLookup {region; _}
| E_Mod       {region; _}
| E_ModPath   {region; _}
| E_Mult      {region; _} -> region
| E_Mutez     t -> t#region
| E_Nat       t -> t#region
| E_Neg       {region; _}
| E_Neq       {region; _} -> region
| E_Nil       t -> t#region
| E_Not       {region; _}
| E_Or        {region; _}
| E_Par       {region; _}
| E_Proj      {region; _}
| E_Record    {region; _}
| E_Set       {region; _}
| E_SetMem    {region; _} -> region
| E_String    t -> t#region
| E_Sub       {region; _}
| E_Tuple     {region; _}
| E_Typed     {region; _}
| E_Update    {region; _} -> region
| E_Var       t
| E_Verbatim  t -> t#region

and tuple_expr_to_region x = x.Region.region

and typed_expr_to_region x = x.Region.region

and record_expr_to_region x = x.Region.region

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
| I_Remove {region; _} -> region
| I_Skip    t -> t#region
| I_While  {region; _} -> region

let test_clause_to_region = function
  ClauseInstr instr -> instr_to_region instr
| ClauseBlock block -> block.Region.region

(* IMPORTANT: In the following function definition, the data
   constructors are sorted alphabetically. If you add or modify some,
   please make sure they remain in order. *)

let pattern_to_region = function
  P_App     {region; _} -> region
| P_Bytes   t -> t#region
| P_Cons    {region; _} -> region
| P_Ctor    t -> t#region
| P_Int     t -> t#region
| P_List    {region; _}
| P_ModPath {region; _} -> region
| P_Mutez    t -> t#region
| P_Nat      t -> t#region
| P_Nil      t -> t#region
| P_Par     {region; _}
| P_Record  {region; _} -> region
| P_String  t -> t#region
| P_Tuple   {region; _}
| P_Typed   {region; _}
  -> region
| P_Var     t -> t#region

let declaration_to_region = function
  D_Const    {region; _}
| D_Fun      {region; _}
| D_Module   {region; _}
| D_ModAlias {region; _}
| D_Type     {region; _} -> region
| D_Directive d -> Directive.to_region d

let selection_to_region = function
  FieldName name -> name#region
| Component w -> w#region
