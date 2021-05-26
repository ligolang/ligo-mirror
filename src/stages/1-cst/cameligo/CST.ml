(* Concrete Syntax Tree (CST) for CameLIGO *)

(* To disable warning about multiply-defined record labels. *)

[@@@warning "-30"]

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region

open Utils
type 'a reg = 'a Region.reg

(* Lexemes *)

type lexeme = string

(* Keywords of CameLIGO *)

(* IMPORTANT: The types are sorted alphabetically, except the generic
   [keyword]. If you add or modify some, please make sure they remain
   in order. *)

type keyword    = Region.t

type kwd_begin  = Region.t
type kwd_else   = Region.t
type kwd_end    = Region.t
type kwd_false  = Region.t
type kwd_fun    = Region.t
type kwd_if     = Region.t
type kwd_in     = Region.t
type kwd_let    = Region.t
type kwd_match  = Region.t
type kwd_mod    = Region.t
type kwd_module = Region.t
type kwd_None   = Region.t
type kwd_not    = Region.t
type kwd_of     = Region.t
type kwd_or     = Region.t
type kwd_rec    = Region.t
type kwd_Some   = Region.t
type kwd_struct = Region.t
type kwd_then   = Region.t
type kwd_true   = Region.t
type kwd_type   = Region.t
type kwd_with   = Region.t

(* Symbols *)

(* IMPORTANT: The types are sorted alphabetically. If you add or
   modify some, please make sure they remain in order. *)

type arrow    = Region.t  (* "->" *)
type conj     = Region.t  (* "&&" *)
type disj     = Region.t  (* "||" *)
type caret    = Region.t  (* "^"  *)
type colon    = Region.t  (* ":"  *)
type comma    = Region.t  (* ","  *)
type cons     = Region.t  (* "::" *)
type dot      = Region.t  (* "."  *)
type equal    = Region.t  (* "="  *)
type geq      = Region.t  (* ">=" *)
type gt       = Region.t  (* ">"  *)
type lbrace   = Region.t  (* "{"  *)
type lbracket = Region.t  (* "["  *)
type leq      = Region.t  (* "=<" *)
type lpar     = Region.t  (* "("  *)
type lt       = Region.t  (* "<"  *)
type minus    = Region.t  (* "-"  *)
type neq      = Region.t  (* "<>" *)
type plus     = Region.t  (* "+"  *)
type rbrace   = Region.t  (* "}"  *)
type rbracket = Region.t  (* "]"  *)
type rpar     = Region.t  (* ")"  *)
type semi     = Region.t  (* ";"  *)
type slash    = Region.t  (* "/"  *)
type times    = Region.t  (* "*"  *)
type vbar     = Region.t  (* "|"  *)
type wild     = Region.t  (* "_"  *)

(* Virtual tokens *)

type eof = Region.t

(* Literals *)

type variable    = string reg
type module_name = string reg
type fun_name    = string reg
type type_name   = string reg
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

(* Unit type and value *)

type the_unit = lpar * rpar

(* The Concrete Syntax Tree *)

type t = {
  decl : declaration nseq;
  eof  : eof
}

and cst = t

(* DECLARATIONS (top-level) *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and declaration =
  D_Directive of Directive.t
| D_Let       of let_decl     reg
| D_Module    of module_decl  reg
| D_ModAlias  of module_alias reg
| D_Type      of type_decl    reg

(* Value declarations (a.k.a. let/let-rec declarations) *)

and let_decl =
  kwd_let * kwd_rec option * let_binding * attribute list

and let_binding = {
  binders  : pattern nseq;
  lhs_type : type_annot option;
  eq       : equal;
  let_rhs  : expr
}

and type_annot = colon * type_expr

(* Type declarations *)

and type_decl = {
  kwd_type  : kwd_type;
  name      : type_name;
  eq        : equal;
  type_expr : type_expr
}

(* Module declarations (structures) *)

and module_decl = {
  kwd_module   : kwd_module;
  name         : module_name;
  eq           : equal;
  kwd_struct   : kwd_struct;
  declarations : declaration nseq;
  kwd_end      : kwd_end
}

(* Declaration of module aliases *)

and module_alias = {
  kwd_module : kwd_module;
  alias      : module_name;
  eq         : equal;
  mod_path   : (module_name, dot) nsepseq
}

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and type_expr =
  T_Ctor    of (type_ctor * t_ctor_args) reg
| T_Fun     of (type_expr * arrow * type_expr) reg
| T_Int     of (lexeme * Z.t) reg
| T_ModPath of type_expr module_path reg
| T_Par     of type_expr par reg
| T_Prod    of cartesian
| T_Record  of field_decl reg ne_injection reg
| T_String  of lexeme reg
| T_Sum     of sum_type reg
| T_Var     of variable
| T_Wild    of wild

(* Constructors *)

and t_ctor_args =
  T_Unary of type_expr
| T_Tuple of type_tuple

and type_tuple = (type_expr, comma) nsepseq par reg

(* Record types *)

and field_decl = {
  field_name : field_name;
  field_type : type_annot;
  attributes : attribute list
}

and 'a ne_injection = {
  compound    : compound option;
  ne_elements : ('a, semi) nsepseq;
  terminator  : semi option;
  attributes  : attribute list
}

and compound =
  BeginEnd of kwd_begin * kwd_end
| Braces   of lbrace * rbrace
| Brackets of lbracket * rbracket

(* Sum types *)

and sum_type = {
  lead_vbar  : vbar option;
  variants   : (variant reg, vbar) nsepseq;
  attributes : attribute list
}

and cartesian = (type_expr, times) nsepseq reg

and variant = {
  ctor       : ctor;
  arg        : (kwd_of * type_expr) option;
  attributes : attribute list
}

(* Module path as a type expression *)

and 'a module_path = {
  module_path : (module_name, dot) nsepseq;
  selector    : dot;
  field       : 'a
}

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and bound_variable = {
  var: variable;
  attributes: attributes;
}

and pattern =
  P_Bytes  of (lexeme * Hex.t) reg
| P_Cons   of (pattern, cons) nsepseq reg
| P_Ctor   of (ctor * pattern option) reg
| P_False  of kwd_false
| P_Int    of (lexeme * Z.t) reg
| P_List   of pattern injection reg
| P_Nat    of (lexeme * Z.t) reg
| P_None   of kwd_None
| P_Par    of pattern par reg
| P_Record of pattern field reg ne_injection reg
| P_Some   of (kwd_Some * pattern) reg
| P_String of string reg
| P_True   of kwd_true
| P_Tuple  of tuple_pattern
| P_Typed  of typed_pattern reg
| P_Unit   of the_unit reg
| PVar      of bound_variable

and tuple_pattern = (pattern, comma) nsepseq par reg

and typed_pattern = {
  pattern    : pattern;
  type_annot : type_annot
}

and 'rhs field =
  Punned   of field_name
| Complete of 'rhs full_field

and 'rhs full_field = {
  field_name : field_name;
  assign     : equal;
  field_rhs  : 'rhs;
  attributes : attribute list
}

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and expr =
  E_Add      of plus bin_op reg                (* "+"   *)
| E_Annot    of annot_expr par reg
| E_Bytes    of (string * Hex.t) reg
| E_Call     of (expr * expr nseq) reg
| E_Cat      of caret bin_op reg               (* "^"   *)
| E_CodeInj  of code_inj reg
| E_Cond     of cond_expr reg
| E_Conj     of conj bin_op reg                (* "&&"  *)
| E_Cons     of cons bin_op reg                (* "::"  *)
| E_Ctor     of (ctor * expr option) reg
| E_Disj     of disj bin_op reg                (* "||"  *)
| E_Div      of slash bin_op reg               (* "/"   *)
| E_Equal    of equal bin_op reg               (* "="   *)
| E_False    of kwd_false
| E_Fun      of fun_expr reg
| E_Geq      of geq bin_op reg                 (* ">="  *)
| E_Gt       of gt bin_op reg                  (* ">"   *)
| E_Int      of (string * Z.t) reg
| E_Leq      of leq bin_op reg                 (* "<="  *)
| E_LetIn    of let_in reg
| E_List     of expr injection reg
| E_Lt       of lt bin_op reg                  (* "<"   *)
| E_Match    of match_expr reg
| E_Mod      of kwd_mod bin_op reg             (* "mod" *)
| E_ModAlias of mod_alias reg
| E_ModIn    of mod_in reg
| E_ModPath  of expr module_path reg
| E_Mult     of times bin_op reg               (* "*"   *)
| E_Mutez    of (string * Z.t) reg
| E_Nat      of (string * Z.t) reg
| E_Neg      of minus un_op reg                (* "-a"  *)
| E_Neq      of neq bin_op reg                 (* "<>"  *)
| E_None     of kwd_None
| E_Not      of kwd_not un_op reg              (* "not" *)
| E_Or       of kwd_or bin_op reg              (* "or"  *)
| E_Par      of expr par reg
| E_Proj     of projection reg
| E_Record   of record_expr reg
| E_Seq      of expr injection reg
| E_Some     of (kwd_Some * expr) reg
| E_String   of string reg
| E_Sub      of minus bin_op reg               (* "a-b" *)
| E_True     of kwd_true
| E_Tuple    of (expr, comma) nsepseq reg
| E_TypeIn   of type_in reg
| E_Unit     of the_unit reg
| E_Update   of update reg
| E_Var      of variable
| E_Verbatim of string reg

and annot_expr = expr * type_annot

and 'a injection = {
  compound   : compound option;
  elements   : ('a, semi) sepseq;
  terminator : semi option
}

and 'a bin_op = {
  op   : 'a;
  arg1 : expr;
  arg2 : expr
}

and 'a un_op = {
  op  : 'a;
  arg : expr
}

and record_expr = expr field reg ne_injection

and projection = {
  struct_name : variable;
  selector    : dot;
  field_path  : (selection, dot) nsepseq
}

and selection =
  FieldName of variable
| Component of (string * Z.t) reg

and update = {
  lbrace   : lbrace;
  record   : path;
  kwd_with : kwd_with;
  updates  : field_path_assignment reg ne_injection reg;
  rbrace   : rbrace
}

and field_path_assignment = {
  field_path : path;
  assignment : equal;
  field_expr : expr
}

and path =
  Name of variable
| Path of projection reg

and match_expr = {
  kwd_match : kwd_match;
  expr      : expr;
  kwd_with  : kwd_with;
  lead_vbar : vbar option;
  cases     : (case_clause reg, vbar) nsepseq reg
}

and case_clause = {
  pattern : pattern;
  arrow   : arrow;
  rhs     : expr
}

and let_in = {
  kwd_let    : kwd_let;
  kwd_rec    : kwd_rec option;
  binding    : let_binding;
  kwd_in     : kwd_in;
  body       : expr;
  attributes : attribute list
}

and type_in = {
  type_decl : type_decl;
  kwd_in    : kwd_in;
  body      : expr
}

and mod_in = {
  mod_decl : module_decl;
  kwd_in   : kwd_in;
  body     : expr
}

and mod_alias = {
  mod_alias : module_alias;
  kwd_in    : kwd_in;
  body      : expr
}

and fun_expr = {
  kwd_fun  : kwd_fun;
  binders  : pattern nseq;
  lhs_type : type_annot option;
  arrow    : arrow;
  body     : expr
}

and cond_expr = {
  kwd_if   : kwd_if;
  test     : expr;
  kwd_then : kwd_then;
  ifso     : expr;
  ifnot    : (kwd_else * expr) option
}

(* Code injection.  Note how the field [language] wraps a region in
   another: the outermost region covers the header "[%<language>" and
   the innermost covers the <language>. *)

and code_inj = {
  language : language reg;
  code     : expr;
  rbracket : rbracket
}

(* Projecting regions from some nodes of the CST *)

let rec last to_region = function
    [] -> Region.ghost
|  [x] -> to_region x
| _::t -> last to_region t

let nsepseq_to_region to_region (hd, tl) =
  Region.cover (to_region hd) (last (to_region <@ snd) tl)

let type_expr_to_region = function
  T_Ctor    {region; _}
| T_Fun     {region; _}
| T_Int     {region; _}
| T_ModPath {region; _}
| T_Par     {region; _}
| T_Prod    {region; _}
| T_Record  {region; _}
| T_String  {region; _}
| T_Sum     {region; _}
| T_Var     {region; _}
| T_Wild    region -> region

let pattern_to_region = function
  P_Bytes  {region; _}
| P_Cons   {region; _}
| P_Ctor   {region; _}
| P_False  region
| P_Int    {region; _}
| P_List   {region; _}
| P_Nat    {region; _}
| P_None   region
| P_Par    {region; _}
| P_Record {region; _}
| P_Some   {region; _}
| P_String {region; _}
| P_True   region
| P_Tuple  {region; _}
| P_Typed  {region; _}
| P_Unit   {region; _}
| P_Var    {var={region;_};_}
  -> region

let expr_to_region = function
  E_Add      {region; _}
| E_Annot    {region; _}
| E_Bytes    {region; _}
| E_Call     {region; _}
| E_Cat      {region; _}
| E_CodeInj  {region; _}
| E_Cond     {region; _}
| E_Conj     {region; _}
| E_Cons     {region; _}
| E_Ctor     {region; _}
| E_Disj     {region; _}
| E_Equal    {region; _}
| E_Div      {region; _}
| E_False    region
| E_Fun      {region; _}
| E_Geq      {region; _}
| E_Gt       {region; _}
| E_Int      {region; _}
| E_Leq      {region; _}
| E_LetIn    {region; _}
| E_List     {region; _}
| E_Lt       {region; _}
| E_Match    {region; _}
| E_Mod      {region; _}
| E_ModAlias {region; _}
| E_ModIn    {region; _}
| E_ModPath  {region; _}
| E_Mult     {region; _}
| E_Mutez    {region; _}
| E_Nat      {region; _}
| E_Neg      {region; _}
| E_Neq      {region; _}
| E_None     region
| E_Not      {region; _}
| E_Or       {region; _}
| E_Par      {region; _}
| E_Proj     {region; _}
| E_Record   {region; _}
| E_Seq      {region; _}
| E_Some     {region; _}
| E_String   {region; _}
| E_Sub      {region; _}
| E_True     region
| E_Tuple    {region; _}
| E_TypeIn   {region; _}
| E_Unit     {region; _}
| E_Update   {region; _}
| E_Var      {region; _}
| E_Verbatim {region; _} -> region

let declaration_to_region = function
  D_Directive d -> Directive.to_region d
| D_Let      {region;_}
| D_Module   {region;_}
| D_ModAlias {region;_}
| D_Type     {region;_} -> region

let selection_to_region = function
  FieldName f -> f.region
| Component c -> c.region

let path_to_region = function
  Name var  -> var.region
| Path path -> path.region
