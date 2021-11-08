(* PRINTING THE TOKENS *)

(* This module produces tokens reconstructed from the Concrete Syntax
   Tree (CST) in the same way they should be produced by the lexer. In
   other words, the leaves of the CST are printed as tokens. This
   enables to test the transmission of the tokens from the lexer to
   the parser. *)

[@@@warning "-42"]
[@@@coverage exclude_file]

(* Internal dependencies *)

open CST

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
open! Region

(* Utilities *)

let sprintf = Printf.sprintf

type           'a nseq = 'a Utils.nseq
type ('a,'sep) nsepseq = ('a,'sep) Utils.nsepseq
type ('a,'sep)  sepseq = ('a,'sep) Utils.sepseq

let swap = Utils.swap

(* STATE *)

(* The printing of the tokens makes use of a threaded data structure:
   the _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) *)

type state = <
  offsets : bool;
  mode    : [`Point | `Byte];
  buffer  : Buffer.t
>

let mk_state ?(buffer=Buffer.create 131) ~offsets mode =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
  end

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration] prints values of type [declaration];

     * the name of a CST constructor, for example, [print_E_Int],
       meaning that the argument of the constructor [CST.E_Int] is
       printed; when some constructors belong to different types, we
       compromise.

     * a generic name, like [print_token] for tokens which are
       keywords or symbols; another example is [print_token_opt] when
       we have an optional token. Or higher-order functions like
       [print_injection], [print_option], [print_nsepseq] etc.

   Another design pattern we used here was to make all pattern
   matching on CST constructors a simple routing function, that is,
   without other logic. For example:

   and print_type_expr state = function
     T_Ctor    t -> print_T_Ctor    state t
   | T_Fun     t -> print_T_Fun     state t
   ...

   This means that those functions can be ignored by the maintainers
   if they know the data constructor. *)

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(* HIGHER-ORDER PRINTERS *)

(* Printing optional values *)

let print_option : state -> (state -> 'a -> unit) -> 'a option -> unit =
  fun state print -> function
         None -> ()
  | Some node -> print state node

(* The functions [print_nsepseq], [print_sepseq] and [print_nseq]
   print values of types [Utils.nsepseq], [Utils.sepseq] and
   [Utils.nseq], respectively. *)

let print_nsepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) nsepseq -> unit =
  fun state print sep (head, tail) ->
    let print_aux (sep_reg, item) =
      let sep_line =
        sprintf "%s: %s\n" (compact state sep_reg) sep in
      Buffer.add_string state#buffer sep_line;
      print state item
    in print state head; List.iter print_aux tail

let print_sepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) sepseq -> unit =
  fun state print sep ->
    print_option state (fun state -> print_nsepseq state print sep)

let print_nseq : state -> (state -> 'a -> unit) -> 'a nseq -> unit =
  fun state print -> Utils.nseq_iter (print state)

(* When printing a sequence, e.g., by means of [nsepseq], whose items
   are of type [Region.reg], the function [strip] is useful to strip
   the items from their region (type [Region.t]). *)

let strip : 'a.(state -> 'a -> unit) -> state -> 'a reg -> unit =
  fun print state node -> print state node.value

(* PRINTING TOKENS (LEAVES) *)

(* Guideline: When destructuring a value [v] of type [Region.t], use
   the following order: [let {value; region} = v in ...]

   The following functions are used by others to make strings,
   vertbatim strings, integers and bytes. See for example functions
   [print_T_String], [print_E_Nat], [print_P_Int] etc.

   Note that, contrary to the following utility functions,
   higher-order printers, like [print_case], [print_conditional]
   etc. are defined when they are first needed.*)

(* Keywords and symbols *)

let print_token state token region =
  let line =
    sprintf "%s: %s\n" (compact state region) token
  in Buffer.add_string state#buffer line

let print_token_opt state lexeme =
  print_option state (swap print_token lexeme)

(* Strings *)

let print_string state {region; value} =
  let line =
    sprintf "%s: String %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

(* Integers and natural numbers *)

let print_int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_nat state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Nat (%S, %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

(* Bytes *)

let print_bytes state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Bytes (%S, \"0x%s\")\n"
            (compact state region) lexeme
            (Hex.show abstract)
  in Buffer.add_string state#buffer line

(* Mutez *)

let print_mutez state {region; value=lex,z} =
  let line =
    sprintf "Mutez %s (%s)" lex (Z.to_string z)
  in print_token state line region

(* Identifiers *)

let print_ident state {region; value} =
  let line =
    sprintf "%s: Ident %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_UIdent state {region; value} =
  let line =
    sprintf "%s: UIdent %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

let print_ctor = print_UIdent

(* Attributes *)

let print_attribute state {region; value} =
  let line =
    sprintf "%s: Attr %S\n" (compact state region) value
  in Buffer.add_string state#buffer line

(* HIGHER-ORDER PRINTERS *)

let print_par :
  state -> (state -> 'a -> unit) -> 'a par reg -> unit =
  fun state print {value; _} ->
    print_token state "LPAR" value.lpar;
    print       state value.inside;
    print_token state "RPAR" value.rpar

let print_brackets :
  state -> (state -> 'a -> unit) -> 'a brackets reg -> unit =
  fun state print ({value; _}: 'a brackets reg) ->
    print_token state "LBRACKET" value.lbracket;
    print       state value.inside;
    print_token state "LBRACKET" value.rbracket

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  print_nseq  state print_declaration node.decl;
  print_token state "EOF" node.eof

(* DECLARATIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration state = function
  D_Const     d -> print_D_Const     state d
| D_Directive d -> print_D_Directive state d
| D_Fun       d -> print_D_Fun       state d
| D_Module    d -> print_D_Module    state d
| D_ModAlias  d -> print_D_ModAlias  state d
| D_Type      d -> print_D_Type      state d

(* Constant declarations *)

and print_D_Const state (node : const_decl reg) =
  let node = node.value in
  print_attributes state node.attributes;
  print_token      state "Const" node.kwd_const;
  print_pattern    state node.pattern;
  print_token      state "EQ" node.equal;
  print_expr       state node.init;
  print_token_opt  state "SEMI" node.terminator

and print_attributes state = List.iter (print_attribute state)

and print_type_annot state (colon, type_expr) =
  print_token     state "COLON" colon;
  print_type_expr state type_expr

(* Preprocessing directives *)

and print_D_Directive state dir =
  let s =
    Directive.to_string ~offsets:state#offsets state#mode dir
  in Buffer.add_string state#buffer s

(* Function declarations *)

and print_D_Fun state (node : fun_decl reg) =
  let node = node.value in
  print_attributes state node.attributes;
  print_token      state "Function" node.kwd_function;
  print_ident      state node.fun_name;
  print_parameters state node.param;
  print_option     state print_type_annot node.ret_type;
  print_token      state "Is" node.kwd_is;
  print_expr       state node.return;
  print_token_opt  state "SEMI" node.terminator;

and print_parameters state (node : parameters) =
  let print state = print_nsepseq state print_param_decl "SEMI"
  in print_par state print node

and print_param_decl state = function
  ParamConst p -> print_ParamConst state p
| ParamVar   p -> print_ParamVar   state p

and print_ParamConst state (node : param_const reg) =
  print_token  state "Const" node.value.kwd_const;
  print_ident  state node.value.var;
  print_option state print_type_annot node.value.param_type

and print_ParamVar state (node : param_var reg) =
  print_token  state "Var" node.value.kwd_var;
  print_ident  state node.value.var;
  print_option state print_type_annot node.value.param_type

(* Module declarations *)

and print_D_Module state (node : module_decl reg) =
  let node = node.value in
  match node.enclosing with
    Braces (kwd_block_opt, lbrace, rbrace) ->
      print_token     state "Module" node.kwd_module;
      print_ident     state node.name;
      print_token     state "Is" node.kwd_is;
      print_token_opt state "BLOCK" kwd_block_opt;
      print_token     state "LBRACE" lbrace;
      print_nseq      state print_declaration node.declarations;
      print_token     state "RBRACE" rbrace;
      print_token_opt state "SEMI" node.terminator
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token     state "Module" node.kwd_module;
      print_ident     state node.name;
      print_token     state "Is" node.kwd_is;
      print_token     state "Begin" kwd_begin;
      print_nseq      state print_declaration node.declarations;
      print_token     state "End" kwd_end;
      print_token_opt state "SEMI" node.terminator

(* Module aliases declarations *)

and print_D_ModAlias state (node : module_alias reg) =
  let node = node.value in
  print_token     state "Module" node.kwd_module;
  print_ident     state node.alias;
  print_token     state "Is" node.kwd_is;
  print_nsepseq   state print_ident "DOT" node.mod_path;
  print_token_opt state "SEMI" node.terminator

(* Type declarations *)

and print_D_Type state (node : type_decl reg) =
  let node = node.value in
  print_token     state "Type" node.kwd_type;
  print_ident     state node.name;
  print_token     state "Is" node.kwd_is;
  print_type_expr state node.type_expr;
  print_token_opt state "SEMI" node.terminator

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_T_Ctor] comes before [print_T_Fun]. *)

and print_type_expr state = function
  T_Ctor    t -> print_T_Ctor    state t
| T_Fun     t -> print_T_Fun     state t
| T_Int     t -> print_T_Int     state t
| T_ModPath t -> print_T_ModPath state t
| T_Par     t -> print_T_Par     state t
| T_Prod    t -> print_T_Prod    state t
| T_Record  t -> print_T_Record  state t
| T_String  t -> print_T_String  state t
| T_Sum     t -> print_T_Sum     state t
| T_Var     t -> print_T_Var     state t
| T_Wild    t -> print_T_Wild    state t

(* General constructors *)

and print_T_Ctor state (node : (type_ctor * type_tuple) reg) =
  let type_name, type_tuple = node.value in
  print_ident      state type_name;
  print_type_tuple state type_tuple

and print_type_tuple state (node : type_tuple) =
  let print state = print_nsepseq state print_type_expr "COMMA"
  in print_par state print node

(* Functional types *)

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let domain, arrow, codomain = node.value in
  print_type_expr state domain;
  print_token     state "ARROW" arrow;
  print_type_expr state codomain

(* The integer type *)

and print_T_Int state = print_int state

(* Module paths *)

and print_T_ModPath state = print_module_path state print_ident

and print_module_path :
  'a.state -> (state -> 'a -> unit) -> 'a module_path reg -> unit =
  fun state print {value; _} ->
    print_nsepseq state print_UIdent "DOT" value.module_path;
    print_token   state "DOT" value.selector;
    print         state value.field

(* Parenthesised type expressions *)

and print_T_Par state = print_par state print_type_expr

(* Product types *)

and print_T_Prod state (node : cartesian) =
  print_nsepseq state print_type_expr "TIMES" node.value

(* Record types *)

and print_T_Record state = print_ne_injection state print_field_decl

and print_field_decl state (node : field_decl reg) =
  let node = node.value in
  print_attributes state node.attributes;
  print_ident      state node.field_name;
  print_option     state print_type_annot node.field_type

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun state print {value; _} ->
    print_attributes       state value.attributes;
    print_ne_injection_kwd state value.kind;
    print_ne_inj_enclosing state print value

and print_ne_inj_enclosing :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print node ->
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "LBRACKET" lbracket;
        print_nsepseq   state print "SEMI" node.ne_elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "RBRACKET" rbracket
    | End kwd_end ->
        print_nsepseq   state print "SEMI" node.ne_elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "End" kwd_end

and print_ne_injection_kwd state = function
  `Map    kwd -> print_token state "Map"    kwd
| `Record kwd -> print_token state "Record" kwd
| `Set    kwd -> print_token state "Set"    kwd

(* The string type *)

and print_T_String state = print_string state

(* Sum types *)

and print_T_Sum state (node : sum_type reg) =
  let node = node.value in
  print_attributes state node.attributes;
  print_token_opt  state "VBAR" node.lead_vbar;
  print_nsepseq    state (strip print_variant) "VBAR" node.variants

and print_variant state (node : variant) =
  print_attributes state node.attributes;
  print_ctor       state node.ctor;
  print_option     state print_of_type_expr node.arg

and print_of_type_expr state (kwd_of, type_expr) =
  print_token     state "Of" kwd_of;
  print_type_expr state type_expr

(* A type variable *)

and print_T_Var state = print_ident state

(* A catch-all variable (a.k.a. joker) *)

and print_T_Wild state = print_token state "WILD"


(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_S_Instr] comes before [print_S_Decl]. *)

and print_statement state = function
  S_Instr   s -> print_S_Instr   state s
| S_Decl    s -> print_S_Decl    state s
| S_VarDecl s -> print_S_VarDecl state s

and print_statements state =
  print_nsepseq state print_statement "SEMI"

and print_S_Instr state = print_instruction state

and print_S_Decl state = print_declaration state

and print_S_VarDecl state (node : var_decl reg) =
  let node = node.value in
  print_token     state "Var" node.kwd_var;
  print_pattern   state node.pattern;
  print_token     state "ASSIGN" node.assign;
  print_expr      state node.init;
  print_token_opt state "SEMI" node.terminator

(* INSTRUCTIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Assign] comes before [print_I_Call]. *)

and print_instruction state = function
  I_Assign      i -> print_I_Assign      state i
| I_Call        i -> print_I_Call        state i
| I_Case        i -> print_I_Case        state i
| I_Cond        i -> print_I_Cond        state i
| I_For         i -> print_I_For         state i
| I_ForIn       i -> print_I_ForIn       state i
| I_MapPatch    i -> print_I_MapPatch    state i
| I_MapRemove   i -> print_I_MapRemove   state i
| I_RecordPatch i -> print_I_RecordPatch state i
| I_Skip        i -> print_I_Skip        state i
| I_SetPatch    i -> print_I_SetPatch    state i
| I_SetRemove   i -> print_I_SetRemove   state i
| I_While       i -> print_I_While       state i

(* Assignments *)

and print_I_Assign state (node : assignment reg) =
  let node = node.value in
  print_lhs   state node.lhs;
  print_token state "ASSIGN" node.assign;
  print_expr  state node.rhs

and print_lhs state = function
  Path    path -> print_path       state path
| MapPath path -> print_map_lookup state path

and print_map_lookup state (node : map_lookup reg) =
  let node = node.value in
  print_path     state node.path;
  print_brackets state print_expr node.index

and print_path state = function
  Name p -> print_Name       state p
| Path p -> print_projection state p

and print_Name state = print_ident state

and print_projection state (node : projection reg) =
  let node = node.value in
  print_ident   state node.struct_name;
  print_token   state "DOT" node.selector;
  print_nsepseq state print_selection "DOT" node.field_path

and print_selection state = function
  FieldName s -> print_FieldName state s
| Component s -> print_Component state s

and print_FieldName state = print_ident state
and print_Component state = print_int state

(* Procedure calls *)

and print_I_Call state = print_call state

and print_call state (node : call) =
  let expr, arguments = node.value in
  print_expr       state expr;
  print_tuple_expr state arguments

and print_tuple_expr state (node : tuple_expr) =
  let print state = print_nsepseq state print_expr "COMMA"
  in print_par state print node

(* Case instructions *)

and print_I_Case state = print_case state print_test_clause

and print_case :
  'a.state -> (state -> 'a -> unit) -> 'a case reg -> unit =
  fun state print {value; _} ->
    print_token          state "Case" value.kwd_case;
    print_expr           state value.expr;
    print_token          state "Of" value.kwd_of;
    print_case_enclosing state print value

and print_case_enclosing :
  'a.state  -> (state -> 'a -> unit) -> 'a case -> unit =
  fun state print node ->
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "LBRACKET" lbracket;
        print_token_opt state "VBAR" node.lead_vbar;
        print_cases     state print node.cases.value;
        print_token     state "RBRACKET" rbracket
    | End kwd_end ->
        print_token_opt state "VBAR" node.lead_vbar;
        print_cases     state print node.cases.value;
        print_token     state "End" kwd_end

and print_cases :
  'a.state -> (state -> 'a -> unit) -> ('a case_clause reg, vbar) nsepseq -> unit =
  fun state print node ->
    let print state node = print_case_clause state print node.value
    in print_nsepseq state print "VBAR" node

and print_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print node ->
    print_pattern state node.pattern;
    print_token   state "ARROW" node.arrow;
    print         state node.rhs

and print_test_clause state = function
  ClauseInstr c -> print_ClauseInstr state c
| ClauseBlock c -> print_ClauseBlock state c

and print_ClauseInstr state = print_instruction state

and print_ClauseBlock state = print_block state

and print_block state (node : block reg) =
  let node = node.value in
  match node.enclosing with
    Braces (kwd_block_opt, lbrace, rbrace) ->
      print_token_opt  state "Block" kwd_block_opt;
      print_token      state "LBRACE" lbrace;
      print_statements state node.statements;
      print_token_opt  state "SEMI" node.terminator;
      print_token      state "RBRACE" rbrace
  | BeginEnd (kwd_begin, kwd_end) ->
      print_token      state "Begin" kwd_begin;
      print_statements state node.statements;
      print_token_opt  state "SEMI" node.terminator;
      print_token      state "End" kwd_end

(* Conditional instructions *)

and print_I_Cond state =
  print_conditional state
                    ~print_ifso:print_test_clause
                    ~print_ifnot:print_test_clause

and print_conditional :
  'ifso 'ifnot.state ->
  print_ifso:(state -> 'ifso -> unit) ->
  print_ifnot:(state -> 'ifnot -> unit) ->
  ('ifso,'ifnot) conditional reg -> unit =
  fun state ~print_ifso ~print_ifnot {value; _} ->
    print_token     state "If" value.kwd_if;
    print_expr      state value.test;
    print_token     state "Then" value.kwd_then;
    print_ifso      state value.ifso;
    print_option    state (swap print_else print_ifnot) value.ifnot

and print_else :
  'a.state -> (state -> 'a -> unit) -> (kwd_else * 'a) -> unit =
  fun state print (kwd_else, ifnot) ->
    print_token state "Else" kwd_else;
    print       state ifnot

(* Bounded iterations on integer intervals (a.k.a. "for loops") *)

and print_I_For state (node : for_int reg) =
  let node = node.value in
  print_token  state "For" node.kwd_for;
  print_ident  state node.binder;
  print_token  state "ASSIGN" node.assign;
  print_expr   state node.init;
  print_token  state "To" node.kwd_to;
  print_expr   state node.bound;
  print_option state print_step node.step;
  print_block  state node.block

and print_step state (kwd_step, expr) =
  print_token state "Step" kwd_step;
  print_expr  state expr

(* Iterations over collections (maps, sets and lists) *)

and print_I_ForIn state (node : for_in reg) =
  let node = node.value in
  print_token      state "For" node.kwd_for;
  print_ident      state node.var;
  print_option     state print_bind_to node.bind_to;
  print_token      state "In" node.kwd_in;
  print_collection state node.collection;
  print_expr       state node.expr;
  print_block      state node.block

and print_bind_to state (arrow, variable) =
  print_token state "ARROW" arrow;
  print_ident state variable

and print_collection state = function
  `List kwd -> print_token state "List" kwd
| `Map  kwd -> print_token state "Map"  kwd
| `Set  kwd -> print_token state "Set"  kwd

(* Map patches *)

and print_I_MapPatch state (node : map_patch reg) =
  let node = node.value in
  print_token        state "Patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "With" node.kwd_with;
  print_ne_injection state print_binding node.map_inj

and print_binding state (node : binding reg) =
  let node = node.value in
  print_expr  state node.source;
  print_token state "ARROW" node.arrow;
  print_expr  state node.image

(* Removal of entries in a map *)

and print_I_MapRemove state (node : map_remove reg) =
  let node = node.value in
  print_token state "Remove" node.kwd_remove;
  print_expr  state node.key;
  print_token state "From" node.kwd_from;
  print_token state "Map" node.kwd_map;
  print_path  state node.map

(* Patching records *)

and print_I_RecordPatch state (node : record_patch reg) =
  let node = node.value in
  print_token        state "Patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "With" node.kwd_with;
  print_ne_injection state (swap print_field print_expr) node.record_inj

(* Skipping (non-operation) *)

and print_I_Skip state = print_token state "Skip"

(* Patching sets *)

and print_I_SetPatch state (node : set_patch reg) =
  let node = node.value in
  print_token        state "Patch" node.kwd_patch;
  print_path         state node.path;
  print_token        state "With" node.kwd_with;
  print_ne_injection state print_expr node.set_inj

(* Removal from sets *)

and print_I_SetRemove state (node : set_remove reg) =
  let node = node.value in
  print_token state "Remove" node.kwd_remove;
  print_expr  state node.element;
  print_token state "From" node.kwd_from;
  print_token state "Set" node.kwd_set;
  print_path  state node.set

(* While loops *)

and print_I_While state (node : while_loop reg) =
  let node = node.value in
  print_token state "While" node.kwd_while;
  print_expr  state node.cond;
  print_block state node.block

(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Bytes] comes before [print_P_Cons]. *)

and print_pattern state = function
  P_Bytes    p -> print_P_Bytes  state p
| P_Cons     p -> print_P_Cons   state p
| P_Ctor     p -> print_P_Ctor   state p
| P_False    p -> print_P_False  state p
| P_Int      p -> print_P_Int    state p
| P_List     p -> print_P_List   state p
| P_Nat      p -> print_P_Nat    state p
| P_Nil      p -> print_P_Nil    state p
| P_None     p -> print_P_None   state p
| P_Par      p -> print_P_Par    state p
| P_Record   p -> print_P_Record state p
| P_Some     p -> print_P_Some   state p
| P_String   p -> print_P_String state p
| P_True     p -> print_P_True   state p
| P_Tuple    p -> print_P_Tuple  state p
| P_Typed    p -> print_P_Typed  state p
| P_Unit     p -> print_P_Unit   state p
| P_Var      p -> print_P_Var    state p

(* Bytes as literals in patterns *)

and print_P_Bytes state = print_bytes state

(* A series of cons operators in patterns *)

and print_P_Cons state (node : (pattern, cons) nsepseq reg) =
  print_nsepseq state print_pattern "SHARP" node.value

(* A constructor application (or constant constructor) in patterns *)

and print_P_Ctor state (node : (ctor * tuple_pattern option) reg) =
  let ctor, arg_opt = node.value in
  print_ctor   state ctor;
  print_option state print_tuple_pattern arg_opt

and print_tuple_pattern state (node : tuple_pattern) =
  let print state = print_nsepseq state print_pattern "COMMA"
  in print_par state print node

(* The Boolean untruth as a pattern *)

and print_P_False state = print_token state "False"

(* Integers in patterns *)

and print_P_Int state = print_int state

(* Patterns of lists by extension *)

and print_P_List state = print_injection state print_pattern

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection reg -> unit =
  fun state print {value; _} ->
    print_injection_kwd state value.kind;
    print_inj_enclosing state print value

and print_inj_enclosing :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print node ->
    match node.enclosing with
      Brackets (lbracket, rbracket) ->
        print_token     state "LBRACKET" lbracket;
        print_sepseq    state print "SEMI" node.elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "RBRACKET" rbracket
    | End kwd_end ->
        print_sepseq    state print "SEMI" node.elements;
        print_token_opt state "SEMI" node.terminator;
        print_token     state "End" kwd_end

and print_injection_kwd state = function
  `BigMap kwd -> print_token state "BigMap" kwd
| `List   kwd -> print_token state "List"   kwd
| `Map    kwd -> print_token state "Map"    kwd
| `Set    kwd -> print_token state "Set"    kwd

(* Natural numbers in patterns *)

and print_P_Nat state = print_nat state

(* The pattern for the empty list *)

and print_P_Nil state = print_token state "Nil"

(* The pattern for the predefined constructor [None] *)

and print_P_None state = print_token state "Ctor_None"

(* The special pattern matching the head of a list and its tail, the
   whole between parentheses. *)

and print_P_Par state = print_par state print_pattern

(* Record patterns *)

and print_P_Record state =
  print_ne_injection state (swap print_field print_pattern)

and print_field :
  'rhs.state -> (state -> 'rhs -> unit) -> 'rhs field reg -> unit =
  fun state print node ->
    match node.value with
      Punned field_name ->
        print_ident state field_name
    | Complete {field_name; assign; field_rhs; attributes} ->
        print_attributes state attributes;
        print_ident      state field_name;
        print_token      state "EQ" assign;
        print            state field_rhs

(* The pattern for the application of the predefined constructor
   [Some] *)

and print_P_Some state (node : (kwd_Some * pattern par reg) reg) =
  let ctor_Some, patterns = node.value in
  print_token state "Ctor_Some" ctor_Some;
  print_par   state print_pattern patterns

(* String literals as patterns *)

and print_P_String state = print_string state

(* The Boolean for truth in patterns *)

and print_P_True state = print_token state "True"

(* The pattern matching a tuple *)

and print_P_Tuple state = print_tuple_pattern state

(* Typed pattern *)

and print_P_Typed state (node : typed_pattern reg) =
  let node = node.value in
  print_pattern    state node.pattern;
  print_type_annot state node.type_annot

(* The pattern matching the unique value of the type "unit". *)

and print_P_Unit state = print_token state "Unit"

(* A pattern variable *)

and print_P_Var state = print_ident state

(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_E_Add] comes before [print_E_And]. *)

and print_expr state = function
  E_Add       e -> print_E_Add       state e
| E_And       e -> print_E_And       state e
| E_BigMap    e -> print_E_BigMap    state e
| E_Block     e -> print_E_Block     state e
| E_Bytes     e -> print_E_Bytes     state e
| E_Call      e -> print_E_Call      state e
| E_Case      e -> print_E_Case      state e
| E_Cat       e -> print_E_Cat       state e
| E_CodeInj   e -> print_E_CodeInj   state e
| E_Equal     e -> print_E_Equal     state e
| E_Cond      e -> print_E_Cond      state e
| E_Cons      e -> print_E_Cons      state e
| E_Ctor      e -> print_E_Ctor      state e
| E_Div       e -> print_E_Div       state e
| E_False     e -> print_E_False     state e
| E_Fun       e -> print_E_Fun       state e
| E_Geq       e -> print_E_Geq       state e
| E_Gt        e -> print_E_Gt        state e
| E_Int       e -> print_E_Int       state e
| E_Leq       e -> print_E_Leq       state e
| E_List      e -> print_E_List      state e
| E_Lt        e -> print_E_Lt        state e
| E_Map       e -> print_E_Map       state e
| E_MapLookup e -> print_E_MapLookup state e
| E_Mod       e -> print_E_Mod       state e
| E_ModPath   e -> print_E_ModPath   state e
| E_Mult      e -> print_E_Mult      state e
| E_Mutez     e -> print_E_Mutez     state e
| E_Nat       e -> print_E_Nat       state e
| E_Neg       e -> print_E_Neg       state e
| E_Nil       e -> print_E_Nil       state e
| E_Neq       e -> print_E_Neq       state e
| E_None      e -> print_E_None      state e
| E_Not       e -> print_E_Not       state e
| E_Or        e -> print_E_Or        state e
| E_Par       e -> print_E_Par       state e
| E_Proj      e -> print_E_Proj      state e
| E_Record    e -> print_E_Record    state e
| E_Set       e -> print_E_Set       state e
| E_SetMem    e -> print_E_SetMem    state e
| E_Some      e -> print_E_Some      state e
| E_String    e -> print_E_String    state e
| E_Sub       e -> print_E_Sub       state e
| E_True      e -> print_E_True      state e
| E_Tuple     e -> print_E_Tuple     state e
| E_Typed     e -> print_E_Typed     state e
| E_Unit      e -> print_E_Unit      state e
| E_Update    e -> print_E_Update    state e
| E_Var       e -> print_E_Var       state e
| E_Verbatim  e -> print_E_Verbatim  state e

(* Arithmetic addition *)

and print_E_Add state = print_op2 state "PLUS"

and print_op2 state lexeme (node : keyword bin_op reg) =
  let node = node.value in
  print_expr  state node.arg1;
  print_token state lexeme node.op;
  print_expr  state node.arg2

(* Boolean conjunction *)

and print_E_And state = print_op2 state "And"

(* Big maps defined intensionally *)

and print_E_BigMap state = print_injection state print_binding

(* Block expressions *)

and print_E_Block state (node : block_with reg) =
  let node = node.value in
  print_block state node.block;
  print_token state "With" node.kwd_with;
  print_expr  state node.expr

(* Bytes as expressions *)

and print_E_Bytes state = print_bytes state

(* Function calls *)

and print_E_Call state = print_call state

(* Case expressions *)

and print_E_Case state = print_case state print_expr

(* String catenation *)

and print_E_Cat state = print_op2 state "CARET"

(* Code Injection *)

(* NOTE: This printer is an exception to the rule. Indeed, we here
   deconstruct this token into its constituents and we print them as
   concrete syntax, instead of tokens. This is hopefully better for
   debugging tokens whose lexemes have been scanned with not trivial
   rules. *)

and print_E_CodeInj state (node : code_inj reg) =
  let {value; region} = node.value.language in
  let header_stop = region#start#shift_bytes 1 in
  let header_reg  = Region.make ~start:region#start ~stop:header_stop in
  print_token  state "[%" header_reg;
  print_string state value;
  print_expr   state node.value.code;
  print_token  state "]" node.value.rbracket

(* Conditional expressions *)

and print_E_Cond state =
  print_conditional state ~print_ifso:print_expr ~print_ifnot:print_expr

(* Consing (that is, pushing an item on top of a stack/list *)

and print_E_Cons state = print_op2 state "SHARP"

(* Constructor application (or constant constructor) as expressions *)

and print_E_Ctor state (node : (ctor * arguments option) reg) =
  let ctor, arguments = node.value in
  print_ctor   state ctor;
  print_option state print_tuple_expr arguments

(* The Euclidean quotient *)

and print_E_Div state = print_op2 state "SLASH"

(* Equality *)

and print_E_Equal state = print_op2 state "EQ"

(* The Boolean untruth *)

and print_E_False state = print_token state "False"

(* Functional expressions *)

and print_E_Fun state (node : fun_expr reg) =
  let node = node.value in
  print_token      state "Function" node.kwd_function;
  print_parameters state node.param;
  print_option     state print_type_annot node.ret_type;
  print_token      state "Is" node.kwd_is;
  print_expr       state node.return

(* Greater or Equal *)

and print_E_Geq state = print_op2 state "Geq"

(* Greater Than *)

and print_E_Gt state = print_op2 state "Gt"

(* Integer literals as expressions *)

and print_E_Int state = print_int state

(* Lower or Equal *)

and print_E_Leq state = print_op2 state "Leq"

(* Lists of expressions defined intensionally *)

and print_E_List state = print_injection state print_expr

(* Lower Than *)

and print_E_Lt state = print_op2 state "Lt"

(* Map expressions defined intensionally (that is, by a series of
   bindings from keys to values. *)

and print_E_Map state = print_injection state print_binding

(* Map lookup as an expression (denoting the key or a failure) *)

and print_E_MapLookup state = print_map_lookup state

(* Euclidean reminder ("modulo") *)

and print_E_Mod state = print_op2 state "Mod"

(* Module path as an expression *)

and print_E_ModPath state = print_module_path state print_expr

(* Arithmetic multiplication *)

and print_E_Mult state = print_op2 state "TIMES"

(* Literal mutez as expressions *)

and print_E_Mutez state = print_mutez state

(* Natural numbers as expressions *)

and print_E_Nat state = print_nat state

(* Arithmetic negation *)

and print_E_Neg state = print_op1 state "MINUS"

and print_op1 state lexeme (node : keyword un_op reg) =
  let node = node.value in
  print_token state lexeme node.op;
  print_expr  state node.arg

(* The empty list as a value *)

and print_E_Nil state = print_token state "Nil"

(* Not Equal *)

and print_E_Neq state = print_op2 state "NE"

(* The predefined constant constructor [None] as an expression *)

and print_E_None state = print_token state "Ctor_None"

(* Boolean negation *)

and print_E_Not state = print_op1 state "Not"

(* Boolean disjunction *)

and print_E_Or state = print_op2 state "Or"

(* Parenthesised expression *)

and print_E_Par state = print_par state print_expr

(* Projections *)

and print_E_Proj state = print_projection state

(* Record expression defined intensionally (that is, by listing all
   the field assignments) *)

and print_E_Record state =
  print_ne_injection state (swap print_field print_expr)

(* Set expression defined intensionally (that is, by listing all the
   elements) *)

and print_E_Set state = print_injection state print_expr

(* Set membership *)

and print_E_SetMem state (node : set_mem reg) =
  let node = node.value in
  print_expr  state node.set;
  print_token state "Contains" node.kwd_contains;
  print_expr  state node.element

(* Application of the predefined constructor [Some] *)

and print_E_Some state (node : (kwd_Some * arguments) reg) =
  let ctor_Some, arguments = node.value in
  print_token      state "Ctor_Some" ctor_Some;
  print_tuple_expr state arguments

(* String literals as expressions *)

and print_E_String state = print_string state

(* Arithmetic subtraction *)

and print_E_Sub state = print_op2 state "MINUS"

(* Boolean truth as an expression *)

and print_E_True state = print_token state "True"

(* Tuples of expressions *)

and print_E_Tuple state = print_tuple_expr state

(* Expressions annotated with a type *)

and print_E_Typed state = print_par state print_typed_expr

and print_typed_expr state (expr, type_annot) =
  print_expr       state expr;
  print_type_annot state type_annot

(* The unique value of the type "unit" *)

and print_E_Unit state = print_token state "Unit"

(* Functional updates of record expressions *)

and print_E_Update state (node : update reg) =
  let node = node.value in
  print_path         state node.record;
  print_token        state "With" node.kwd_with;
  print_ne_injection state print_field_path_assignment
                     node.updates

and print_field_path_assignment state (node : field_path_assignment reg) =
  let node = node.value in
  print_path  state node.field_path;
  print_token state "EQ" node.assign;
  print_expr  state node.field_expr

(* Expression variables *)

and print_E_Var state = print_ident state

(* Verbatim strings as expressions *)

and print_E_Verbatim state = print_verbatim state

(* Printing tokens (client-slide) *)

type ('src, 'dst) printer = state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer
let print_to_string state cst = print_to_buffer state cst |> Buffer.contents

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
