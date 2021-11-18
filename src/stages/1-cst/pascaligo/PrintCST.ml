(* PRINTING THE CST *)

(* This module produces a textual representation of a subset of the
   Concrete Abstract Tree (CST). It aims at a readable format with the
   most relevant nodes, with source locations. This functionality is
   most useful when testing the parser, for example, checking that a
   particular node corresponding to an operator has the expected
   associativity with the same kind or the expected priority over
   another. *)

[@@@warning "-42"]
[@@@coverage exclude_file]

(* Internal dependencies *)

open CST

module Tree = Cst_shared.Tree
open Tree
(*type state = Lexing_shared.Wrap.state*)

(* Vendor dependencies *)

module Directive = LexerLib.Directive
module Utils     = Simple_utils.Utils
module Region    = Simple_utils.Region
open! Region

(* Utilities *)

let sprintf = Printf.sprintf

let (<@) = Utils.(<@) (* Function composition operator *)

type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq

(* The function [swap] takes a function and two values which are
   applied to the function in reverse order. It is useful when calling
   a function whose arguments have no labels and we want to do an eta
   expansion. For example,

   [let mk_child print value = Some (swap print value)]

   instead of

   [let mk_child print value = Some (fun state -> print state value)].

   We also use [swap] in arguments to calls to higher-order functions,
   for instance:

   [mk_child (swap print_then print) value.ifso]
 *)

let swap = Utils.swap

(* PRINTING THE CST *)

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration] prints values of type [declaration]; it can
       also be the type in a region, like [val print_variant : state
       -> variant reg -> unit], instead of [val print_variant : state
       -> variant -> unit];

     * the name of a CST constructor, for example, [print_E_Int],
       meaning that the argument of the constructor [CST.E_Int] is
       printed;

     * a generic name, like [print_token] for tokens which are
       keywords or symbols; another example is [print_token_opt] when
       we have an optional token.

   Another design pattern we used here was to make all pattern
   matching on CST constructors a simple routing function, that is,
   devoid of logic. For example:

   and print_type_expr state = function
     T_App    t -> print_T_App state t
   | T_Fun    t -> print_T_Fun state t
   ...

   This means that those functions can be ignored by the maintainers
   if they know the data constructor. *)

(* Guideline: When destructuring a value [v] of type [Region.t], use
   the following order: [let {value; region} = v in ...] *)

let rec print_cst state (node : cst) =
  let children =
     List.map (mk_child print_declaration)
  @@ Utils.nseq_to_list node.decl
  in print_tree state "<cst>" children

(* DECLARATIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_D_Const] comes before [print_D_Fun]. *)

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
  let children = [
    mk_child      print_pattern         node.pattern;
    mk_child_opt  print_type_annotation node.const_type;
    mk_child      print_expr            node.init;
    mk_child_list print_attributes      node.attributes]
  in print_tree state "D_Const" children

and print_type_annotation state =
  print_unary state "<type>" print_type_expr <@ snd

and print_attributes state =
  print_tree state "<attributes>" <@ List.map (mk_child print_attribute)

and print_attribute state (node : Wrap.attribute reg) =
  let key, val_opt = node.value in
  match val_opt with
    None -> print_unary state "<attribute>" print_node key
  | Some AttrString value ->
     let children = [
         mk_child print_XXX (* TODO *)
       ]
     in print_tree

(* Preprocessing directives *)

and print_D_Directive state dir =
  print_unary state "Directive" print_string (Directive.project dir)

(* Function declarations *)

and print_D_Fun state (node : fun_decl reg) =
  let node = node.value in
  let children = [
     mk_child_opt  print_recursive  node.kwd_recursive;
     mk_child      print_literal    node.fun_name;
     mk_child      print_parameters node.param;
     mk_child_opt  print_ret_type   node.ret_type;
     mk_child      print_ret_expr   node.return;
     mk_child_list print_attributes node.attributes]
  in print_tree state "D_Fun" children

and print_recursive state _ = print_short state "recursive"

and print_parameters state (node : parameters) =
  let children =
     List.map (mk_child print_param_decl)
  @@ Utils.nsepseq_to_list node.value.inside
  in print_tree state "<parameters>" children

and print_param_decl state (node : param_decl reg) =
  let {value; region} = node in
  let children = [
    mk_child print_param_kind      value.param_kind;
    mk_child print_var_pattern     value.var;
    mk_child print_type_annotation value.param_type]
  in print_tree state "<parameter>" ~region children

and print_param_kind state = function
  `Var   kwd_var   -> print_literal state kwd_var
| `Const kwd_const -> print_literal state kwd_const

and print_var_pattern state (node : var_pattern reg) =
  let node = node.value in
  if value.attributes = [] then
    print_literal state node.variable
  else
    let children = [
      mk_child      print_literal    node.variable;
      mk_child_list print_attributes node.attributes]
    in mk_tree state "<attributed variable>" children

and print_ret_type state =
  print_unary state "<return type>" print_type_expr <@ snd

and print_ret_expr state =
  print_unary state "<return expression>" print_expr

(* Module declarations *)

and print_D_Module state (node : module_decl reg) =
  let node = node.value in
  let children = [
    mk_child print_literal      node.name;
    mk_child print_declarations node.declarations]
  in print_tree state "D_Module" children

and print_declarations state =
  print_tree state "<structure>"
  <@ List.map (mk_child print_declaration)
  <@ Utils.nseq_to_list

(* Module aliases declarations *)

and print_D_ModAlias state (node : module_alias reg) =
  let node = node.value in
  let children = [
    mk_child print_literal  node.alias;
    mk_child print_mod_path node.mod_path]
  in print_tree state "D_ModAlias" children

and print_mod_path state =
  print_tree state "<path>"
  <@ List.map (mk_child print_literal)
  <@ Utils.nsepseq_to_list

(* Type declarations *)

and print_D_Type state (node : type_decl reg) =
  let node = node.value in
  let print_type_expr state =
    print_unary state "<type>" print_type_expr in
  let children = [
    mk_child     print_literal   node.name;
    mk_child_opt print_type_vars node.params;
    mk_child     print_type_expr node.type_expr]
  in print_tree state "D_Type" children

and print_type_vars state (node : type_vars) =
  let {value; region} = node in
  let children =
    List.map (mk_child print_literal)
    @@ Utils.nsepseq_to_list value.inside
  in print_tree state "<type variables>" ~region children

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_T_App] comes before [print_T_Fun]. *)

and print_type_expr state = function
  T_Cart    t -> print_T_Cart    state t
| T_App    t -> print_T_App    state t
| T_Fun     t -> print_T_Fun     state t
| T_Int     t -> print_T_Int     state t
| T_ModPath t -> print_T_ModPath state t
| T_Par     t -> print_T_Par     state t
| T_Record  t -> print_T_Record  state t
| T_String  t -> print_T_String  state t
| T_Sum     t -> print_T_Sum     state t
| T_Var     t -> print_T_Var     state t

(* Cartesian products *)

and print_T_Cart state (node : cartesian) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_type_expr)
  @@ Utils.nsepseq_to_list value
  in print_tree state "T_Cart" ~region children

(* Application of type constructors *)

and print_T_App state (node : type_ctor_app reg) =
  let {value = (path, tuple); region} = node in
  let path = path.value in
  let print_path state =
    print_module_path state ~root:"<module path>" print_literal in
  let children = [
    mk_child print_path       path;
    mk_child print_type_tuple tuple]
  in print_tree state "T_App" ~region children

and print_module_path :
  'a.state -> root:string -> (state -> 'a -> unit) -> 'a module_path reg -> unit =
  fun state ~root print {value; region} ->
    let children =
      (List.map (mk_child print_literal)
      @@ Utils.nsepseq_to_list value.module_path)
      @ [mk_child print value.field]
    in print_tree state root ~region children

and print_type_tuple state (node : (type_expr, comma) nsepseq par reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_type_expr)
  @@ Utils.nsepseq_to_list value.inside
  in print_tree state "<type arguments>" ~region children

(* Functional types *)

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let {value = (domain, _, codomain); region} = node in
  let children = [
    mk_child print_type_expr domain;
    mk_child print_type_expr codomain]
  in print_tree state "T_Fun" ~region children

(* The integer type *)

and print_T_Int state = print_int state "T_Int"

(* Module paths *)

and print_T_ModPath state (node : type_name module_path reg) =
  let {value; region} = node in
  let children =
    (List.map (mk_child print_literal)
    @@ Utils.nsepseq_to_list value.module_path)
    @ [mk_child print_string value.field]
  in print_tree state "T_ModPath" ~region children

(* Parenthesised type expressions *)

and print_T_Par state (node : type_expr par reg) =
  print_unary state "T_Par" print_type_expr node.value.inside

(* Record types *)

and print_T_Record state (node : field_decl reg compound reg) =
  let node = node.value in
  let fields =
     List.map (mk_child print_field_decl)
  @@ Utils.nsepseq_to_list node.elements
  and attributes =
    [mk_child_list print_attributes node.attributes]
  in print_tree state "T_Record" (fields @ attributes)

and print_field_decl state (node : field_decl reg) =
  let node = node.value in
  let children = [
    mk_child_opt  print_type_annotation node.field_type;
    mk_child_list print_attributes      node.attributes] in
  let {value; region} = node.field_name in
  print_tree state value ~region children

(* The string type *)

and print_T_String state =
  print_unary state "T_String" print_string

(* Sum types *)

and print_T_Sum state (node : sum_type reg) =
  let node = node.value in
  let variants =
    List.map (mk_child print_variant)
  @@ Utils.nsepseq_to_list node.variants in
  let attributes =
    [mk_child_list print_attributes node.attributes]
  in print_tree state "T_Sum" (variants @ attributes)

and print_variant state (node : variant reg) =
  let node = node.value in
  let {value; region} = node.ctor in
  let children = [
    mk_child_opt  print_of_type_expr node.args;
    mk_child_list print_attributes   node.attributes]
  in print_tree state value ~region children

and print_of_type_expr state (_, type_expr) =
  print_type_expr state type_expr

(* A type variable *)

and print_T_Var state = print_unary state "T_Var" print_literal


(* STATEMENTS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_S_Instr] comes before [print_S_Decl]. *)

and print_statement state = function
  S_Instr   i -> print_S_Instr   state i
| S_Decl    i -> print_S_Decl    state i
| S_VarDecl i -> print_S_VarDecl state i

and print_statements state =
  print_tree state "<statements>"
  <@ List.map (mk_child print_statement)
  <@ Utils.nsepseq_to_list

and print_S_Instr state =
  print_unary state "S_Instr" print_instruction

and print_S_Decl state =
  print_unary state "S_Decl" print_declaration

and print_S_VarDecl state (node : var_decl reg) =
  let node = node.value in
  let children = [
    mk_child      print_pattern         node.pattern;
    mk_child_opt  print_type_annotation node.var_type;
    mk_child      print_expr            node.init;
    mk_child_list print_attributes      node.attributes]
  in print_tree state "S_VarDecl" children

(* INSTRUCTIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Assign] comes before [print_I_Call]. *)

and print_instruction state = function
  I_Assign i -> print_I_Assign state i
| I_Call   i -> print_I_Call   state i
| I_Case   i -> print_I_Case   state i
| I_Cond   i -> print_I_Cond   state i
| I_For    i -> print_I_For    state i
| I_ForIn  i -> print_I_ForIn  state i
| I_Patch  i -> print_I_Patch  state i
| I_Remove i -> print_I_Remove state i
| I_Skip   i -> print_I_Skip   state i
| I_While  i -> print_I_While  state i

(* Assignments *)

and print_I_Assign state (node : assignment reg) =
  let {value; region} = node
  and print_lhs state = print_unary state "<lhs>" print_expr
  and print_rhs state = print_unary state "<rhs>" print_expr in
  let children = [
    mk_child print_lhs value.lhs;
    mk_child print_rhs value.rhs]
  in print_tree state "I_Assign" ~region children

(* Procedure calls *)

and print_I_Call state = print_call state "I_Call"

and print_call state label (node : call) =
  let {value = (func, args); region} = node

  and mk_func state =
    print_unary state "<function>" print_expr

  and mk_args state (node : (expr, comma) nsepseq par reg) =
    let children =
       List.map (mk_child print_expr)
    @@ Utils.nsepseq_to_list node.value.inside
    in print_tree state "<arguments>" children in

  let children = [
    mk_child mk_func func;
    mk_child mk_args args]
  in print_tree state label ~region children

(* Case instructions *)

and print_I_Case state = print_case state "I_Case" print_test_clause

and print_case :
  'a.state -> string -> (state -> 'a -> unit) -> 'a case reg -> unit =
  fun state label print {value; region} ->
    let print_case_test state =
      print_unary state "<condition>" print_expr in

    let cases =
      List.map (mk_child @@ swap print_case_clause print)
    @@ Utils.nsepseq_to_list value.cases in

    let children =
      mk_child print_case_test value.expr :: cases
    in print_tree state label ~region children

and print_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause reg -> unit =
  fun state print {value; _} ->
    let print_clause_pattern state =
      print_unary state "<pattern>" print_pattern in
    let children = [
      mk_child print_clause_pattern value.pattern;
      mk_child print                value.rhs]
    in print_tree state "<clause>" children

and print_test_clause state = function
  ClauseInstr c -> print_ClauseInstr state c
| ClauseBlock c -> print_ClauseBlock state c

and print_ClauseInstr state =
  print_unary state "ClauseInstr" print_instruction

and print_ClauseBlock state =
  print_unary state "ClauseBlock" print_block

and print_block state (node : block reg) =
  let node = node.value in
  let children =
     List.map (mk_child print_statement)
  @@ Utils.nsepseq_to_list node.statements
  in print_tree state "<block>" children

(* Conditional instructions *)

and print_I_Cond state =
  print_conditional state "I_Cond"
                    ~print_ifso:print_test_clause
                    ~print_ifnot:print_test_clause

and print_conditional :
  'ifso 'ifnot.
  state ->
  string ->
  print_ifso:(state -> 'ifso -> unit) ->
  print_ifnot:(state -> 'ifnot -> unit) ->
  ('ifso,'ifnot) conditional reg -> unit =
  fun state label ~print_ifso ~print_ifnot {value; region} ->
    let print_cond state = print_unary state "<condition>" print_expr
    and print_then state = print_unary state "<true>" print_ifso
    and print_else state = print_unary state "<false>" print_ifnot <@ snd in
    let children = [
      mk_child     print_cond value.test;
      mk_child     print_then value.ifso;
      mk_child_opt print_else value.ifnot]
    in print_tree state label ~region children

(* Iterations on integer intervals *)

and print_I_For state (node : for_int reg) =
  let {value; region} = node in

  let print_init state (index, init : variable * expr) =
    let children = [
      mk_child print_literal index;
      mk_child print_expr init]
    in print_tree state "<init>" children

  and print_bound state =
    print_unary state "<bound>" print_expr

  and print_step state (_, expr) =
    print_unary state "<step>" print_expr expr in

  let children = [
    mk_child     print_init  (value.index, value.init);
    mk_child     print_bound value.bound;
    mk_child_opt print_step  value.step;
    mk_child     print_block value.block]
  in print_tree state "I_For" ~region children

(* Iterations over collections (maps, sets and lists) *)

and print_I_ForIn state (node : for_in reg) =
  let {value; region} = node in

  let print_var_binding state (source, image) =
    let children = [
      mk_child print_literal source;
      mk_child print_literal image]
    in print_tree state "<binding>" children in

  let print_index state = function
    index,  None ->
      print_literal state index
  | source, Some (_, image) ->
      print_var_binding state (source, image) in

  let children = [
    mk_child print_index      (value.var, value.bind_to);
    mk_child print_collection value.expr;
    mk_child print_block      value.block]
  in print_tree state "I_ForIn" ~region children

and print_collection state =
  print_unary state "<collection>" print_expr

(* Patches *)

and print_I_Patch state (node : patch reg) =
  let {value; region} = node in
  let children = [
    mk_child print_collection node.collection;
    mk_child print_patch      node.patch]
  in print_tree state "I_Patch" ~region children

and print_patch state = print_expr state

(**)

(* Removal of entries in a map *)

and print_I_MapRemove state (node : map_remove reg) =
  let {value; region}  = node
  and print_key  state = print_unary state "<key>" print_expr
  and print_path state = print_unary state "<map>" print_path in
  let children = [
    mk_child print_key  value.key;
    mk_child print_path value.map]
  in print_tree state "I_MapRemove" ~region children



and print_map_lookup state label (node : map_lookup reg) =
  let {value; region} = node in

  let print_path state = print_unary state "<map>" print_path

  and print_index state (index : expr brackets reg) =
    print_unary state "<index>" print_expr index.value.inside in

  let children = [
    mk_child print_path  value.path;
    mk_child print_index value.index]

  in print_tree state label ~region children

(* Skipping (non-operation) *)

and print_I_Skip state wrap =
  print_node state "I_Skip" wrap#payload

(* Removal from sets *)

and print_I_SetRemove state (node : set_remove reg) =
  let {value; region} = node in
  let children = [
    mk_child print_expr value.element;
    mk_child print_path value.set]
  in print_tree state "I_SetRemove" ~region children

and print_path state = function
  Name name ->
    print_unary state "Name" print_literal name
| Path path ->
    let {value; region} = path in
    let children =
      mk_child print_literal value.struct_name
      :: (List.map (mk_child print_selection)
          @@ Utils.nsepseq_to_list value.field_path)
    in print_tree state "Path" ~region children

and print_selection state = function
  FieldName name -> print_FieldName state name
| Component comp -> print_Component state comp

and print_FieldName state =
  print_unary state "FieldName" print_literal

and print_Component state (node : (lexeme * Z.t) reg) =
  let {value = (lexeme, z); region} = node in
  let children = [
    mk_child print_literal  {value=lexeme; region};
    mk_child print_short (Z.to_string z)]
  in print_tree state "Component" children

(* While loops *)

and print_I_While state (node : while_loop reg) =
  let {value; _} = node in
  let children = [
    mk_child print_cond  value.cond;
    mk_child print_block value.block]
  in print_tree state "<while>" children

and print_cond state = print_unary state "<condition>" print_expr

and print_block_expr state =
  print_unary state "<expr>" print_expr


(* PATTERNS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_I_Bytes] comes before [print_P_Cons]. *)

and print_pattern state = function
  P_Bytes   p -> print_P_Bytes   state p
| P_Cons    p -> print_P_Cons    state p
| P_App     p -> print_P_App    state p
| P_Int     p -> print_P_Int     state p
| P_List    p -> print_P_List    state p
| P_Nat     p -> print_P_Nat     state p
| P_Nil     p -> print_P_Nil     state p
| P_Par     p -> print_P_Par     state p
| P_Record  p -> print_P_Record  state p
| P_String  p -> print_P_String  state p
| P_Tuple   p -> print_P_Tuple   state p
| P_Typed   p -> print_P_Typed   state p
| P_Var     p -> print_P_Var     state p

(* Bytes as literals in patterns *)

and print_P_Bytes state = print_bytes state "P_Bytes"

(* A series of cons operators in patterns *)

and print_P_Cons state (node : (pattern, cons) nsepseq reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_pattern)
  @@ Utils.nsepseq_to_list value
  in print_tree state "P_Cons" ~region children

(* A constructor application (or constant constructor) in patterns *)

and print_P_App state (node : (ctor * tuple_pattern option) reg) =
  let {value = (ctor, tuple); region} = node in
  let children = [
    mk_child     print_literal                           ctor;
    mk_child_opt (swap print_ctor_args print_pattern) tuple]
  in print_tree state "P_App" ~region children

and print_ctor_args :
  'a.state -> (state -> 'a -> unit) -> ('a, comma) nsepseq par reg -> unit =
  fun state print node ->
  let children =
     List.map (mk_child print)
  @@ Utils.nsepseq_to_list node.value.inside
  in print_tree state "<arguments>" children

(* Integers in patterns *)

and print_P_Int state = print_int state "P_Int"

(* Patterns of lists by extension *)

and print_P_List state (node : pattern compound reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_pattern)
  @@ Utils.sepseq_to_list value.elements
  in print_tree state "P_List" ~region children

(* Natural numbers in patterns *)

and print_P_Nat state = print_int state "P_Nat"

(* The pattern for the empty list *)

and print_P_Nil state = print_literal' state "P_Nil"

(* Parenthesised patterns *)

and print_P_Par state (node : pattern par reg) =
  print_unary state "P_Par" print_pattern node.value.inside

(* Record patterns *)

and print_P_Record state (node : pattern field reg compound reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child @@ swap print_field print_pattern)
  @@ Utils.nsepseq_to_list value.elements
  in print_tree state "P_Record" ~region children

and print_field :
  'lhs 'rhs.state ->
  (state -> 'lhs -> unit) ->
  (state -> 'rhs -> unit) ->
  ('lhs, 'rhs) field reg ->
  unit =
  fun state print_lhs print_rhs {value; region} ->
    match value with
      Punned {pun; attributes} ->
      let children = [
        mk_child      print_lhs        pun;
        mk_child_list print_attributes attributes]
      in print_tree state "<punned field>" ~region children
    | Complete {field_lhs; field_rhs; attributes; _} ->
        let children = [
          mk_child      print_lhs        field_lhs;
          mk_child      print_rhs        field_rhs;
          mk_child_list print_attributes attributes]
        in print_tree state "<field>" ~region children

(* String literals as patterns *)

and print_P_String state = print_unary state "P_String" print_string

(* The pattern matching a tuple *)

and print_P_Tuple state (node : (pattern, comma) nsepseq par reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_pattern)
  @@ Utils.nsepseq_to_list value.inside
  in print_tree state "P_Tuple" ~region children

(* Typed pattern *)

and print_P_Typed state (node : typed_pattern reg) =
  let {value; region} = node in
  let {pattern; type_annot} = value in
  let children = [
    mk_child print_pattern pattern;
    mk_child print_type_annotation type_annot]
  in print_tree state "P_Typed" ~region children

(* A pattern variable *)

and print_P_Var state = print_unary state "P_Var" print_literal


(* EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. Note
   that the sections below follow the alphabetical order too:
   [print_E_Add] comes before [print_E_And]. *)

and print_expr state = function
  E_Add       e -> print_E_Add       state e
| E_And       e -> print_E_And       state e
| E_App       e -> print_E_App      state e
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
| E_Div       e -> print_E_Div       state e
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
| E_Not       e -> print_E_Not       state e
| E_Or        e -> print_E_Or        state e
| E_Par       e -> print_E_Par       state e
| E_Proj      e -> print_E_Proj      state e
| E_Record    e -> print_E_Record    state e
| E_Set       e -> print_E_Set       state e
| E_SetMem    e -> print_E_SetMem    state e
| E_String    e -> print_T_String    state e
| E_Sub       e -> print_E_Sub       state e
| E_Tuple     e -> print_E_Tuple     state e
| E_Typed     e -> print_E_Typed     state e
| E_Update    e -> print_E_Update    state e
| E_Var       e -> print_E_Var       state e
| E_Verbatim  e -> print_E_Verbatim  state e

(* Arithmetic addition *)

and print_E_Add state = print_op2 state "E_Add"

and print_op2 state label {value; region} =
  let children = [
    mk_child print_expr value.arg1;
    mk_child print_expr value.arg2]
  in print_tree state label ~region children

(* Boolean conjunction *)

and print_E_And state = print_op2 state "E_And"

(* Big maps defined intensionally *)

and print_E_BigMap state (node : binding reg compound reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_binding)
  @@ Utils.sepseq_to_list value.elements
  in print_tree state "E_BigMap" ~region children

(* Block expressions *)

and print_E_Block state (node : block_with reg) =
  let {value; region} = node in
  let children = [
    mk_child print_block      value.block;
    mk_child print_block_expr value.expr]
  in print_tree state "E_Block" ~region children

(* Bytes as expressions *)

and print_E_Bytes state = print_bytes state "E_Bytes"

(* Function calls *)

and print_E_Call state = print_call state "E_Call"

(* Case expressions *)

and print_E_Case state = print_case state "E_Case" print_expr

(* String catenation *)

and print_E_Cat state = print_op2 state "E_Cat"

(* Code Injection *)

and print_E_CodeInj state (node : code_inj reg) =
  let {value; region} = node in
  let children = [
    mk_child print_language value.language.value;
    mk_child print_code     value.code]
  in print_tree state "E_CodeInj" ~region children

and print_language state =
  print_unary state "<language>" print_E_String

and print_code state = print_unary state "<code>" print_expr

(* Equality *)

and print_E_Equal state = print_op2 state "E_Equal"

(* Conditional expressions *)

and print_E_Cond state =
  print_conditional state "E_Cond"
                    ~print_ifso:print_expr
                    ~print_ifnot:print_expr

(* Consing (that is, pushing an item on top of a stack/list *)

and print_E_Cons  state = print_op2 state "E_Cons"

(* Constructor application (or constant constructor) as expressions *)

and print_E_App state (node :  (ctor * arguments option) reg) =
  let {value; region} = node in
  let ctor, args = value in
  let children = [
    mk_child     print_literal ctor;
    mk_child_opt (swap print_ctor_args print_expr) args]
  in print_tree state ctor.value ~region children

(* The Euclidean quotient *)

and print_E_Div state = print_op2 state "E_Div"

(* The Boolean untruth *)

and print_E_False state = print_literal' state "E_False"

(* Functional expressions *)

and print_E_Fun state (node : fun_expr reg) =
  let node = node.value in
  let children = [
     mk_child     print_parameters node.param;
     mk_child_opt print_ret_type   node.ret_type;
     mk_child     print_ret_expr   node.return]
  in print_tree state "E_Fun" children

(* Greater or Equal *)

and print_E_Geq state = print_op2 state "E_Geq"

(* Greater Than *)

and print_E_Gt state = print_op2 state "E_Gt"

(* Integer literals as expressions *)

and print_E_Int state = print_unary state state "E_Int"

(* Lower or Equal *)

and print_E_Leq state = print_op2 state "E_Leq"

(* Lists of expressions defined intensionally *)

and print_E_List state (node : expr compound reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_expr)
  @@ Utils.sepseq_to_list value.elements
  in print_tree state "E_List" ~region children

(* Lower Than *)

and print_E_Lt state = print_op2 state "E_Lt"

(* Map expressions defined intensionally (that is, by a series of
   bindings from keys to values. *)

and print_E_Map state (node : binding reg compound reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_binding)
  @@ Utils.sepseq_to_list value.elements
  in print_tree state "E_Map" ~region children

(* Map lookup as an expression (denoting the key or a failure) *)

and print_E_MapLookup state = print_map_lookup state "E_MapLookup"

(* Euclidean reminder ("modulo") *)

and print_E_Mod state = print_op2 state "E_Mod"

(* Module path as an expression *)

and print_E_ModPath state (node : expr module_path reg) =
  print_module_path state ~root:"E_ModPath" print_expr node

(* Arithmetic multiplication *)

and print_E_Mult state = print_op2 state "E_Mult"

(* Literal mutez as expressions *)

and print_E_Mutez state _ = print_short state "E_Mutez"

(* Natural numbers as expressions *)

and print_E_Nat state = print_int state "E_Nat"

(* Arithmetic negation *)

and print_E_Neg state = print_op1 state "E_Neg"

and print_op1 state label {value; region} =
  print_unary state label ~region print_expr value.arg

(* The empty list as a value *)

and print_E_Nil state = print_literal' state "E_Nil"

(* Not Equal *)

and print_E_Neq state = print_op2 state "E_Neq"

(* Boolean negation *)

and print_E_Not state = print_op1 state "E_Not"

(* Boolean disjunction *)

and print_E_Or state = print_op2 state "E_Or"

(* Parenthesised expression *)

and print_E_Par state (node : expr par reg) =
  let {value; region} = node in
  print_unary state "E_Par" ~region print_expr value.inside

(* Projections *)

and print_E_Proj state (node : projection reg) =
  let {value; region} = node in
  let children =
    mk_child print_literal value.struct_name
    :: (List.map (mk_child print_selection)
        @@ Utils.nsepseq_to_list value.field_path)
  in print_tree state "E_Proj" ~region children

(* Record expression defined intensionally (that is, by listing all
   the field assignments) *)

and print_E_Record state (node : record_expr reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child @@ swap print_field print_expr)
  @@ Utils.nsepseq_to_list value.elements
  in print_tree state "E_Record" ~region children

(* Set expression defined intensionally (that is, by listing all the
   elements) *)

and print_E_Set state (node : expr compound reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_expr)
  @@ Utils.sepseq_to_list value.elements
  in print_tree state "E_Set" ~region children

(* Set membership *)

and print_E_SetMem state (node : set_mem reg) =
  let {value; region} = node
  and print_set state = print_unary state "<set>" print_expr
  and print_elt state = print_unary state "<element>" print_expr in
  let children = [
    mk_child print_set value.set;
    mk_child print_elt value.element]
  in print_tree state "E_SetMem" ~region children

(* String literals as expressions *)

and print_E_String state =
  print_unary state "E_String" print_string

(* Arithmetic subtraction *)

and print_E_Sub state = print_op2 state "E_Sub"

(* Tuples of expressions *)

and print_E_Tuple state (node : (expr, comma) nsepseq par reg) =
  let {value; region} = node in
  let children =
    List.map (mk_child print_expr)
    @@ Utils.nsepseq_to_list value.inside
  in print_tree state "E_Tuple" ~region children

(* Expressions annotated with a type *)

and print_E_Typed state (node : typed_expr par reg) =
  let {value; region} = node in
  let expr, annotation = value.inside in
  let children = [
    mk_child print_expr expr;
    mk_child print_type_annotation annotation]
  in print_tree state "E_Typed" ~region children

(* Functional updates of record expressions *)

and print_E_Update state (node : update reg) =
  let {value; region} = node in
  let inj = value.updates.value in

  let print_update state (node : field_path_assignment reg) =
    let node = node.value in
    let children = [
      mk_child print_path node.field_path;
      mk_child print_expr node.field_expr]
    in print_tree state "<update>" ~region children in

  let assignments =
    List.map (mk_child print_update)
    @@ Utils.nsepseq_to_list inj.elements in
  let attributes =
    [mk_child_list print_attributes inj.attributes]

  in print_tree state "E_Update" ~region (assignments @ attributes)

(* Expression variables *)

and print_E_Var state = print_unary state "E_Var" print_literal

(* Verbatim strings as expressions *)

and print_E_Verbatim state =
  print_unary state "E_Verbatim" print_verbatim

(* Printing tokens (client-slide) *)

type ('src, 'dst) printer = state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer
let print_to_string state cst = print_to_buffer state cst |> Buffer.contents

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
