(* PRINTING THE CST *)

(* This module produces a textual representation of a subset of the
   Concrete Abstract Tree (CST). It aims at a readable format with the
   most relevant nodes, with source locations. This functionality is
   most useful when testing the parser, for example, checking that a
   particular node corresponding to an operator has the expected
   associativity with the same kind or the expected priority over
   another. *)

[@@@coverage exclude_file]

open CST

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
open! Region

let sprintf = Printf.sprintf

(*
let some x = Some x
type 'a nseq = 'a Utils.nseq *)

let (<@) = Utils.(<@)

type ('a, 'sep) nsepseq = ('a, 'sep) Utils.nsepseq

(* The function [swap] takes a function and two values which are
   applied to the function in reverse order. It is useful when calling
   a function whose arguments have no labels and we want to do an eta
   expansion. For example, [swap print_case_clause print] is short for
   [fun state -> print_case_clause state print]. Note: This does not
   work in mutually recursive definitions, because of the value
   restriction of OCaml, so, for example [and print_E_Call = swap
   print_call "E_Call"] would be rejected by the OCaml compiler. *)

let swap = Utils.swap

(* STATE *)

(* The printing of the CST makes use of a threaded data structure: the
   _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) The method [pad] updates the current padding,
   which is comprised of two components: the padding to reach the new
   node (space before reaching a subtree, then a vertical bar for it)
   and the padding for the new node itself. (Is it the last child of
   its parent?) *)

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ?(buffer=Buffer.create 131) ~offsets mode  =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ") >}
  end

(* The names of the printing functions are all prefixed by
   "print_". The rest of the name is either

     * the name of the type whose value is printed, for example
       [print_declaration], [print_attributes], [print_statements]
       etc.;

     * the name of a token, for example, [print_Int];

TODO: Really?

     * then name of a constructor followed by "_arg", for example,
       "print_E_Ctor_arg" means that the argument of the constructor
       [E_Ctor] is printed, whereas, for example, [print_E_Int] would
       mean that the constructor itself would be printed, because it
       denotes a token;

     * a generic name, like [print_token] for tokens which are
       keywords or symbols; another example is [print_token_opt] when
       we have an optional keyword. Or higher-order functions like
       [print_injection], [print_option], [print_nsepseq] etc. *)

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(* Higher-order printers *)

(* When printing a sequence, e.g., by means of [nsepseq], whose items
   are of type [Region.reg], the function [strip] is useful to strip
   the items from their region (type [Region.t]). *)

let strip : 'a.(state -> 'a -> unit) -> state -> 'a reg -> unit =
  fun print state node -> print state node.value

let print_ident state {value; region} =
  let reg  = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path value reg
  in Buffer.add_string state#buffer node

let print_loc_node state value region =
  print_ident state {value; region}

let print_node state name =
  let node = sprintf "%s%s\n" state#pad_path name
  in Buffer.add_string state#buffer node

let print_root ?region state label =
  match region with
           None -> print_node state label
  | Some region -> print_loc_node state label region

let mk_child print value = Some (swap print value)

let mk_child_opt print = function
        None -> None
| Some value -> mk_child print value

let mk_child_list print = function
    [] -> None
| list -> mk_child print list

let print_forest state forest =
  let forest = List.filter_map (fun x -> x) forest in
  let arity  = List.length forest in
  let apply rank print = print (state#pad arity rank)
  in List.iteri apply forest

let print_multi state label ?region children =
  print_root   state label ?region;
  print_forest state children

let print_multi_reg state {value; region} children =
  print_root   state value ~region;
  print_forest state children

let print_unary state label ?region print_sub node =
  print_multi state label ?region [mk_child print_sub node]

(* PRINTING TOKENS (LEAVES) *)

(* Strings *)

let print_string state {value; region} =
  let reg  = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path value reg
  in Buffer.add_string state#buffer node

let print_E_String state =
  print_unary state "E_String" print_string

let print_P_String state =
  print_unary state "P_String" print_string

let print_T_String state =
  print_unary state "T_String" print_string

let print_verbatim state {value; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path value reg
  in Buffer.add_string state#buffer node

let print_E_Verbatim state =
  print_unary state "E_Verbatim" print_verbatim

(* Integers and natural numbers *)

let print_int state label {value=(lexeme, z); region} =
  let children = [
    mk_child print_ident {value=lexeme; region};
    mk_child print_node  (Z.to_string z)]
  in print_multi state label children

let print_T_Int = swap print_int "T_Int"
and print_P_Int = swap print_int "P_Int"
and print_E_Int = swap print_int "E_Int"

and print_P_Nat = swap print_int "P_Nat"
and print_E_Nat = swap print_int "E_Nat"

(* Bytes *)

let print_bytes state label {value=(lexeme, hex); region} =
  let children = [
    mk_child print_ident {value=lexeme; region};
    mk_child print_node  (Hex.show hex)]
  in print_multi state label children

let print_P_Bytes = swap print_bytes "P_Bytes"
and print_E_Bytes = swap print_bytes "E_Bytes"

(* PRINTING THE CST *)

let rec print_cst state (node : cst) =
  let children =
     List.map (mk_child print_declaration)
  @@ Utils.nseq_to_list node.decl
  in print_multi state "<cst>" children

(* Declarations *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration state = function
  D_Const    d -> print_D_Const    state d
| D_Fun      d -> print_D_Fun      state d
| D_Module   d -> print_D_Module   state d
| D_ModAlias d -> print_D_ModAlias state d
| D_Type     d -> print_D_Type     state d

(* Constant declarations *)

and print_type_annot state (node : type_annot) =
  let _, type_expr = node in
  print_unary state "<type>" print_type_expr type_expr

and print_D_Const state (node : const_decl reg) =
  let node = node.value in
  let children = [
    mk_child      print_ident      node.name;
    mk_child_opt  print_type_annot node.const_type;
    mk_child      print_expr       node.init;
    mk_child_list print_attributes node.attributes]
  in print_multi state "D_Const" children

and print_attributes state =
  print_multi state "<attributes>" <@ List.map (mk_child print_ident)

(* Function declarations *)

and print_D_Fun state (node : fun_decl reg) =
  let node = node.value in
  let children = [
     mk_child_opt  print_recursive  node.kwd_recursive;
     mk_child      print_ident      node.fun_name;
     mk_child      print_parameters node.param;
     mk_child_opt  print_ret_type   node.ret_type;
     mk_child      print_ret_expr   node.return;
     mk_child_list print_attributes node.attributes]
  in print_multi state "D_Fun" children

and print_recursive state _ = print_node state "recursive"

and print_parameters state (node : parameters) =
  let children =
     List.map (mk_child print_param_decl)
  @@ Utils.nsepseq_to_list node.value.inside
  in print_multi state "<parameters>" children

and print_param_decl state = function
  ParamConst d -> print_ParamConst state d
| ParamVar   d -> print_ParamVar   state d

and print_ParamConst state (node : param_const reg) =
  let {value; region} = node in
  let children = [
    mk_child     print_ident      value.var;
    mk_child_opt print_type_annot value.param_type]
  in print_multi state "ParamConst" ~region children

and print_ParamVar state (node : param_var reg) =
  let {value; region} = node in
  let children = [
    mk_child     print_ident      value.var;
    mk_child_opt print_type_annot value.param_type]
  in print_multi state "ParamVar" ~region children

and print_ret_type state (node : type_annot) =
  let _, type_expr = node in
  print_unary state "<return type>" print_type_expr type_expr

and print_ret_expr state =
  print_unary state "<return expression>" print_expr

(* Module declarations *)

and print_D_Module state (node : module_decl reg) =
  let node = node.value in
  let children = [
    mk_child print_ident        node.name;
    mk_child print_declarations node.declarations]
  in print_multi state "D_Module" children

and print_declarations state =
  print_multi state "<structure>"
  <@ List.map (mk_child print_declaration)
  <@ Utils.nseq_to_list

(* Module aliases declarations *)

and print_D_ModAlias state (node : module_alias reg) =
  let node = node.value in
  let children = [
    mk_child print_ident    node.alias;
    mk_child print_mod_path node.mod_path]
  in print_multi state "D_ModAlias" children

and print_mod_path state =
  print_multi state "<path>"
  <@ List.map (mk_child print_ident)
  <@ Utils.nsepseq_to_list

(* Type declarations *)

and print_D_Type state (node : type_decl reg) =
  let node = node.value in
  let print_type_expr state =
    print_unary state "<type>" print_type_expr in
  let children = [
    mk_child print_ident     node.name;
    mk_child print_type_expr node.type_expr]
  in print_multi state "D_Type" children

(* TYPE EXPRESSIONS *)

and print_type_expr state = function
  T_Ctor    e -> print_T_Ctor    state e
| T_Fun     e -> print_T_Fun     state e
| T_Int     e -> print_T_Int     state e
| T_ModPath e -> print_T_ModPath state e
| T_Par     e -> print_T_Par     state e
| T_Prod    e -> print_T_Prod    state e
| T_Record  e -> print_T_Record  state e
| T_String  e -> print_T_String  state e
| T_Sum     e -> print_T_Sum     state e
| T_Var     e -> print_T_Var     state e
| T_Wild    e -> print_T_Wild    state e

and print_T_Wild state = print_unary state "T_Wild" print_wild
and print_T_Var  state = print_unary state "T_Var"  print_ident

and print_T_Par state (node : type_expr par reg) =
  print_unary state "T_Par" print_type_expr node.value.inside

and print_T_ModPath state (node : type_expr module_path reg) =
  let node = node.value in
  let children = [
    mk_child print_ident     node.module_name;
    mk_child print_type_expr node.field]
  in print_multi state "T_ModPath" children

and print_T_Record state (node : field_decl reg ne_injection reg) =
  let node = node.value in
  let fields =
     List.map (mk_child print_field_decl)
  @@ Utils.nsepseq_to_list node.ne_elements
  and attributes =
    [mk_child_list print_attributes node.attributes]
  in print_multi state "T_Record" (fields @ attributes)

and print_wild state = print_loc_node state "_"

and print_T_Ctor state (node : (type_ctor * type_tuple) reg) =
  let {value = (name, tuple); region} = node in
  let children = [
    mk_child print_ident      name;
    mk_child print_type_tuple tuple]
  in print_multi state "T_Ctor" ~region children

and print_T_Fun state (node : (type_expr * arrow * type_expr) reg) =
  let {value = (domain, _, codomain); region} = node in
  let children = [
    mk_child print_type_expr domain;
    mk_child print_type_expr codomain]
  in print_multi state "T_Fun" ~region children

and print_T_Sum state (node : sum_type reg) =
  let node = node.value in
  let variants =
    List.map (mk_child (strip print_variant))
  @@ Utils.nsepseq_to_list node.variants in
  let attributes =
    [mk_child_list print_attributes node.attributes]
  in print_multi state "T_Sum" (variants @ attributes)

and print_T_Prod state (node : cartesian) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_type_expr)
  @@ Utils.nsepseq_to_list value
  in print_multi state "T_Prod" ~region children

and print_variant state (node : variant) =
  let {value; region} = node.ctor in
  let children = [
    mk_child_opt  print_of_type_expr node.arg;
    mk_child_list print_attributes   node.attributes]
  in print_multi state value ~region children

and print_of_type_expr state = print_type_expr state <@ snd

and print_field_decl state (node : field_decl reg) =
  let {value; _} = node in
  let children = [
    mk_child      print_field_type value.field_type;
    mk_child_list print_attributes value.attributes]
  in print_multi_reg state value.field_name children

and print_field_type state =
  print_unary state "<type>" print_type_expr

and print_type_tuple
      state (node : (type_expr, comma) nsepseq par reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_type_expr)
  @@ Utils.nsepseq_to_list value.inside
  in print_multi state "<arguments>" ~region children

and print_E_Fun state (node : fun_expr reg) =
  let node = node.value in
  let children = [
     mk_child     print_parameters node.param;
     mk_child_opt print_ret_type   node.ret_type;
     mk_child     print_ret_expr   node.return]
  in print_multi state "E_Fun" children

and print_E_CodeInj state (node : code_inj reg) =
  let {value; region} = node in
  let children = [
    mk_child print_language value.language.value;
    mk_child print_code     value.code]
  in print_multi state "E_CodeInj" ~region children

and print_language state =
  print_unary state "<language>" print_E_String

and print_code state = print_unary state "<code>" print_expr

(* Blocks *)

and print_E_Block state (node : block_with reg) =
  let {value; region} = node in
  let children = [
    mk_child print_block      value.block.value;
    mk_child print_block_expr value.expr]
  in print_multi state "E_Block" ~region children

and print_block state (node : block) =
  let children =
     List.map (mk_child print_statement)
  @@ Utils.nsepseq_to_list node.statements
  in print_multi state "<block>" children

and print_block_expr state =
  print_unary state "<expr>" print_expr

(* Statements *)

and print_statements state =
  print_multi state "<statements>"
  <@ List.map (mk_child print_statement)
  <@ Utils.nsepseq_to_list

and print_statement state = function
  S_Instr   i -> print_S_Instr   state i
| S_Decl    i -> print_S_Decl    state i
| S_VarDecl i -> print_S_VarDecl state i

and print_S_Instr state =
  print_unary state "S_Instr" print_instruction

and print_S_Decl state =
  print_unary state "S_Decl" print_declaration

and print_S_VarDecl state (node : var_decl reg) =
  print_unary state "S_VarDecl" print_var_decl node.value

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_instruction state = function
  I_Assign      e -> print_I_Assign      state e
| I_Call        e -> print_I_Call        state e
| I_Case        e -> print_I_Case        state e
| I_Cond        e -> print_I_Cond        state e
| I_For         e -> print_I_For         state e
| I_ForIn       e -> print_I_ForIn       state e
| I_MapPatch    e -> print_I_MapPatch    state e
| I_MapRemove   e -> print_I_MapRemove   state e
| I_RecordPatch e -> print_I_RecordPatch state e
| I_Skip        e -> print_I_Skip        state e
| I_SetPatch    e -> print_I_SetPatch    state e
| I_SetRemove   e -> print_I_SetRemove   state e
| I_While       e -> print_I_While       state e

(* Removal from sets *)

and print_I_SetRemove state (node : set_remove reg) =
  let {value; region} = node in
  let children = [
    mk_child print_expr value.element;
    mk_child print_path value.set]
  in print_multi state "I_SetRemove" ~region children

(* Patching sets *)

and print_I_SetPatch state (node : set_patch reg) =
  let {value; region} = node in
  let path = mk_child print_path value.path in
  let fields =
     List.map (mk_child print_expr)
  @@ Utils.nsepseq_to_list value.set_inj.value.ne_elements
  in print_multi state "I_Setatch" ~region (path :: fields)

(* Skipping (non-operation) *)

and print_I_Skip state (node : kwd_skip) =
  print_root state ~region:node "I_Skip"

(* Patching records *)

and print_I_RecordPatch state (node : record_patch reg) =
  let {value; region} = node in
  let path = mk_child print_path value.path in
  let fields =
     List.map (mk_child (strip print_field_assignment))
  @@ Utils.nsepseq_to_list value.record_inj.value.ne_elements
  in print_multi state "I_RecordPatch" ~region (path :: fields)

and print_field_assignment state (node : field_assignment) =
  let children = [
    mk_child print_ident node.field_name;
    mk_child print_expr  node.field_expr]
  in print_multi state "<field assignment>" children

(* Removal of entries in a map *)

and print_I_MapRemove state (node : map_remove reg) =
  let {value; region} = node in
  let print_key state =
    print_unary state "<key>" print_expr
  and print_path state =
    print_unary state "<map>" print_path in
  let children = [
    mk_child print_key value.key;
    mk_child print_path value.map]
  in print_multi state "I_MapRemove" ~region children

(* Map patches *)

and print_I_MapPatch state (node : map_patch reg) =
  let {value; region} = node in
  let bindings =
    List.map (mk_child (strip print_binding))
  @@ Utils.nsepseq_to_list value.map_inj.value.ne_elements in
  let children =
    mk_child print_path value.path :: bindings
  in print_multi state "I_MapPatch" ~region children

(* Iterations over collections (maps, sets and lists) *)

and print_I_ForIn state (node : for_in reg) =
  let {value; region} = node in

  let print_var_binding state (source, image) =
    let children = [
      mk_child print_ident source;
      mk_child print_ident image]
    in print_multi state "<binding>" children in

  let print_index state = function
    index,  None ->
      print_ident state index
  | source, Some (_, image) ->
      print_var_binding state (source, image) in

  let print_collection state = function
    `Map  kwd -> print_loc_node state "map"  kwd
  | `Set  kwd -> print_loc_node state "set"  kwd
  | `List kwd -> print_loc_node state "list" kwd in

  let print_collection state (collection, expr : collection * expr) =
    let children = [
      mk_child print_collection collection;
      mk_child print_expr       expr]
    in print_multi state "<collection>" children in

  let children = [
    mk_child print_index      (value.var, value.bind_to);
    mk_child print_collection (value.collection, value.expr);
    mk_child print_statements value.block.value.statements]
  in print_multi state "I_ForIn" ~region children

(* Function and procedure calls *)

and print_E_Call state = print_call state "E_Call"
and print_I_Call state = print_call state "I_Call"

and print_call state label (node : fun_call) =
  let {value = (func, args); region} = node

  and mk_func state =
    print_unary state "<function>" print_expr

  and mk_args state (node : (expr, comma) nsepseq par reg) =
    let children =
       List.map (mk_child print_expr)
    @@ Utils.nsepseq_to_list node.value.inside
    in print_multi state "<arguments>" children in

  let children = [
    mk_child mk_func func;
    mk_child mk_args args]
  in print_multi state label ~region children

(* Assignments *)

and print_I_Assign state (node : assignment reg) =
  let {value; region} = node

  and print_lhs state (node : lhs) =
    let print state : lhs -> unit = function
      Path    p -> print_unary      state "Path" print_path p
    | MapPath p -> print_map_lookup state "MapPath" p
    in print_unary state "<lhs>" print node

  and print_rhs state = print_unary state "<rhs>" print_expr

  in print_multi state "I_Assign" ~region
                 [mk_child print_lhs value.lhs;
                  mk_child print_rhs value.rhs]

and print_map_lookup state label (node : map_lookup reg) =
  let {value; region} = node in

  let print_path state (path : path) =
    print_unary state "<map>" print_path path

  and print_index state (index : expr brackets reg) =
    print_unary state "<index>" print_expr index.value.inside in

  let children = [
    mk_child print_path  value.path;
    mk_child print_index value.index]

  in print_multi state label ~region children

(* While loops *)

and print_I_While state (node : while_loop reg) =
  let {value; _} = node in
  let children = [
    mk_child print_cond  value.cond;
    mk_child print_block value.block.value]
  in print_multi state "<while>" children

and print_cond state = print_unary state "<condition>" print_expr

(* Cases as instructions or expressions *)

and print_I_Case state = print_case state "I_Case" print_test_clause
and print_E_Case state = print_case state "E_Case" print_expr

and print_case :
  'a.state -> string -> (state -> 'a -> unit) -> 'a case reg -> unit =
  fun state label print {value; region} ->
    let print_case_test state =
      print_unary state "<condition>" print_expr in

    let cases =
      List.map (mk_child @@ swap print_case_clause print)
      @@ Utils.nsepseq_to_list
      @@ Utils.nsepseq_map (fun x -> x.value) value.cases.value in

    let children =
      mk_child print_case_test value.expr :: cases
    in print_multi state label ~region children

and print_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print clause ->
    let print_clause_pattern state =
      print_unary state "<pattern>" print_pattern in
    let children = [
      mk_child print_clause_pattern clause.pattern;
      mk_child print                clause.rhs]
    in print_multi state "<clause>" children

(* Conditionals as expressions or instructions *)

and print_I_Cond state = print_conditional state "I_Cond" print_test_clause
and print_E_Cond state = print_conditional state "E_Cond" print_expr

and print_conditional :
  'a.state -> string -> (state -> 'a -> unit) -> 'a conditional reg -> unit =
  fun state label print {value; region} ->

    let print_condition state =
      print_unary state "<condition>" print_expr

    and print_then : 'a.state -> (state -> 'a -> unit) -> 'a -> unit =
      fun state print -> print_unary state "<true>" print

    and print_else : 'a.state -> (state -> 'a -> unit) -> 'a -> unit =
      fun state print -> print_unary state "<false>" print in

    let children = [
      mk_child print_condition         value.test;
      mk_child (swap print_then print) value.ifso;
      mk_child (swap print_else print) value.ifnot]

    in print_multi state label ~region children

and print_test_clause state = function
  ClauseInstr instr -> print_ClauseInstr state instr
| ClauseBlock block -> print_ClauseBlock state block

and print_ClauseInstr state =
  print_unary state "ClauseInstr" print_instruction

and print_ClauseBlock state =
  print_unary state "ClauseBlock" print_clause_block

and print_clause_block state = function
  LongBlock  b -> print_LongBlock  state b
| ShortBlock b -> print_ShortBlock state b

and print_LongBlock state (node : block reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_statement)
  @@ Utils.nsepseq_to_list value.statements
  in print_multi state "LongBlock" ~region children

and print_ShortBlock state (node : (statements * semi option) braces reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_statement)
  @@ Utils.nsepseq_to_list (fst value.inside)
  in print_multi state "ShortBlock" ~region children

(* Patterns *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_pattern state = function
  P_Bytes   p -> print_P_Bytes   state p
| P_Cons    p -> print_P_Cons    state p
| P_Ctor    p -> print_P_Ctor    state p
| P_False   p -> print_P_False   state p
| P_Int     p -> print_P_Int     state p
| P_List    p -> print_P_List    state p
| P_Nat     p -> print_P_Nat     state p
| P_Nil     p -> print_P_Nil     state p
| P_None    p -> print_P_None    state p
| P_ParCons p -> print_P_ParCons state p
| P_Some    p -> print_P_Some    state p
| P_String  p -> print_P_String  state p
| P_True    p -> print_P_True    state p
| P_Tuple   p -> print_P_Tuple   state p
| P_Unit    p -> print_P_Unit    state p
| P_Var     p -> print_P_Var     state p
| P_Wild    p -> print_P_Wild    state p

and print_P_False state = print_loc_node state "P_False"
and print_P_Nil   state = print_loc_node state "P_Nil"
and print_P_None  state = print_loc_node state "P_None"
and print_P_True  state = print_loc_node state "P_True"
and print_P_Unit  state = print_loc_node state "P_Unit"
and print_P_Var   state = print_unary state "P_Var" print_ident
and print_P_Wild  state = print_unary state "P_Wild" print_wild

and print_P_Tuple state (node : (pattern, comma) nsepseq par reg) =
  let region = node.region in
  print_unary state "P_Tuple" ~region print_tuple_pattern node

and print_P_List state (node : pattern injection reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child print_pattern)
  @@ Utils.sepseq_to_list value.elements
  in print_multi state "P_List" ~region children

and print_P_Some state (node : (kwd_Some * pattern par reg) reg) =
  let {value = (_, arg); region} = node in
  print_unary state "P_Some" ~region print_pattern arg.value.inside

and print_P_Ctor state (node : (ctor * tuple_pattern option) reg) =
  let {value = (ctor, tuple); region} = node in
  let children = [
    mk_child     print_ident ctor;
    mk_child_opt print_ctor_arg tuple]
  in print_multi state "P_Ctor" ~region children

and print_ctor_arg state (node :  (pattern, comma) nsepseq par reg) =
  let children =
     List.map (mk_child print_pattern)
  @@ Utils.nsepseq_to_list node.value.inside
  in print_multi state "<arguments>" children

and print_P_Cons state {value; region} =
  let patterns = Utils.nsepseq_to_list value in
  let length   = List.length patterns in
  let apply len rank =
    print_pattern (state#pad len rank) in
  print_loc_node state "P_Cons" region;
  List.iteri (apply length) patterns

and print_P_ParCons state (node : (pattern * cons * pattern) par reg) =
  let {value; region} = node in
  let head, _, tail = value.inside in
  let children = [
    mk_child print_pattern head;
    mk_child print_pattern tail]
  in print_multi state "P_ParCons" ~region children

and print_tuple_pattern
      state (node :  (pattern, comma) nsepseq par reg) =
  let node = node.value in
  let patterns       = Utils.nsepseq_to_list node.inside in
  let length         = List.length patterns in
  let apply len rank = print_pattern (state#pad len rank)
  in List.iteri (apply length) patterns

and print_path state = function
  Name name ->
    print_unary state "Name" print_ident name
| Path {value; region} ->
    print_unary state "Path" ~region print_projection value

and print_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = print_selection (state#pad len rank) in
  print_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri (apply len) selections

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print inj ->
    let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
    let length      = List.length ne_elements in
    let arity      = if inj.attributes = [] then length else length + 1
    and apply len rank = print (state#pad len rank)
    in List.iteri (apply arity) ne_elements;
       if inj.attributes <> [] then
         let state = state#pad arity (arity-1)
         in print_attributes state inj.attributes

and print_update state update =
  print_path         (state#pad 2 0) update.record;
  print_ne_injection state print_field_path_assignment update.updates.value

and print_selection state = function
  FieldName name -> print_FieldName state name
| Component comp -> print_Component state comp

and print_FieldName state =
  print_unary state "FieldName" print_ident

and print_Component state (node : (lexeme * Z.t) reg) =
  let {value=(lexeme, z); region} = node in
  let children = [
    mk_child print_ident {value=lexeme; region};
    mk_child print_node  (Z.to_string z)]
  in print_multi state "Component" children

and print_E_MapLookUp state = print_map_lookup state "E_MapLookUp"

and print_I_For state (node : for_int reg) =
  let {value; region} = node in

  let print_init state (binder, init : variable * expr) =
    let children = [
      mk_child print_ident binder;
      mk_child print_expr  init]
    in print_multi state "<init>" children

  and print_bound state =
    print_unary state "<bound>" print_expr

  and print_step state (_, expr) =
    print_unary state "<step>" print_expr expr

  and print_block state (node : block reg) =
    print_statements state node.value.statements in

  let children = [
    mk_child     print_init  (value.binder, value.init);
    mk_child     print_bound value.bound;
    mk_child_opt print_step  value.step;
    mk_child     print_block value.block]
  in print_multi state "I_For" ~region children

and print_field_path_assignment state (node : field_path_assignment reg) =
  print_node state "<update>";
  print_path (state#pad 2 0) node.value.field_path;
  print_expr (state#pad 2 1) node.value.field_expr

and print_binding state (node : binding) =
  let children = [
    mk_child print_expr node.source;
    mk_child print_expr node.image]
  in print_multi state "<binding>" children

and print_var_decl state (node : var_decl) =
  let arity = if node.var_type = None then 2 else 3
  and rank  = 0 in
  let rank  = print_ident (state#pad arity rank) node.name; rank+1 in
  let rank  = rank (* TODO : print_type_annot (state#pad arity rank) rank node.var_type*)
  in print_expr (state#pad arity rank) node.init

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_expr state = function
  E_Add       e -> print_op2 state "E_Add" e
| E_And       e -> print_op2 state "E_And"   e
| E_Annot     e -> print_annot_expr state e
| E_BigMap    e -> print_E_BigMap state e
| E_Block     e -> print_E_Block state e
| E_Bytes     e -> print_E_Bytes state e
| E_Call      e -> print_E_Call state e
| E_Case      e -> print_E_Case state e
| E_Cat       e -> print_op2 state "E_Cat" e
| E_CodeInj   e -> print_E_CodeInj state e
| E_Equal     e -> print_op2 state "E_Equal" e
| E_Cond      e -> print_E_Cond state e
| E_Cons      e -> print_op2 state "E_Cons" e
| E_Ctor      e -> print_E_Ctor state e
| E_Div       e -> print_op2 state "E_Div" e
| E_False     e -> print_loc_node state "False" e
| E_Fun       e -> print_E_Fun state e
| E_Geq       e -> print_op2 state "E_Geq"   e
| E_Gt        e -> print_op2 state "E_Gt"    e
| E_Int       e -> print_E_Int state e
| E_Leq       e -> print_op2 state "E_Leq"   e
| E_List      e -> print_list_comp state e
| E_Lt        e -> print_op2 state "E_Lt"    e
| E_Map       e -> print_E_Map state e
| E_MapLookUp e -> print_E_MapLookUp state e
| E_Mod       e -> print_op2 state "E_Mod" e
| E_ModPath   e -> print_E_ModPath state e
| E_Mult      e -> print_op2 state "E_Mult" e
| E_Mutez     e -> print_E_Mutez state e
| E_Nat       e -> print_E_Nat state e
| E_Neg       e -> print_op1 state "E_Neg" e
| E_Nil       e -> print_loc_node  state "E_Nil" e
| E_Neq       e -> print_op2 state "E_Neq"   e
| E_None      e -> print_loc_node state "E_None" e
| E_Not       e -> print_op1 state "E_Not"   e
| E_Or        e -> print_op2 state "E_Or"    e
| E_Par       e -> print_E_Par state e
| E_Proj      e -> print_proj_expr state e
| E_Record    e -> print_E_Record state e
| E_Set       e -> print_set_injection state e
| E_SetMem    e -> print_set_mem state e
| E_Some      e -> print_some_app state e
| E_String    e -> print_T_String state e
| E_Sub       e -> print_op2 state "E_Sub" e
| E_True      e -> print_loc_node state "True"  e
| E_Tuple     e -> print_E_Tuple state e
| E_Unit      e -> print_loc_node state "E_Unit" e
| E_Update    e -> print_E_Update state e
| E_Var       e -> print_E_Var state e
| E_Verbatim  e -> print_E_Verbatim state e

and print_E_Mutez state _ = print_node state "E_Mutez"

and print_E_Var state = print_unary state "E_Var" print_ident

and print_E_Update state (node : update reg) =
  let {value; region} = node in
  print_loc_node state "E_Update" region;
  print_update   state value

and print_E_Tuple state (node : (expr, comma) nsepseq par reg) =
  print_node       state "E_Tuple";
  print_tuple_expr state node

and print_E_Map state (node : binding reg injection reg) =
  let {value; region} = node in
  print_loc_node  state "E_Map" region;
  print_injection state (strip print_binding) value

and print_E_BigMap state (node : binding reg injection reg) =
  let {value; region} = node in
  let children =
     List.map (mk_child (strip print_binding))
  @@ Utils.sepseq_to_list value.elements
  in print_multi state "E_BigMap" ~region children

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print inj ->
    print_forest state
    @@ List.map (mk_child print)
    @@ Utils.sepseq_to_list inj.elements

and print_E_Par state (node : expr par reg) =
  let {value; region} = node in
  print_unary state "E_Par" ~region print_expr value.inside

and print_tuple_expr state (node : (expr, comma) nsepseq par reg) =
  let exprs          = Utils.nsepseq_to_list node.value.inside in
  let length         = List.length exprs in
  let apply len rank = print_expr (state#pad len rank)
  in List.iteri (apply length) exprs

and print_E_ModPath state (node : expr module_path reg) =
  let node = node.value in
  let children = [
    mk_child print_ident node.module_name;
    mk_child print_expr node.field]
  in print_multi state "E_ModPath" children

and print_proj_expr state (node : projection reg) =
  print_loc_node   state "E_Proj" node.region;
  print_projection state node.value

and print_E_Record state (node : record reg) =
  print_loc_node     state "E_Record" node.region;
  print_ne_injection state (strip print_field_assignment) node.value

and print_annot_expr state (node : annot_expr par reg) =
  let expr, (_, type_expr) = node.value.inside in
  print_loc_node  state "E_Annot" node.region;
  print_expr      (state#pad 2 0) expr;
  print_type_expr (state#pad 2 1) type_expr

and print_list_comp state (node : expr injection reg) =
  print_loc_node state "E_List" node.region;
  if node.value.elements = None then
    print_node (state#pad 1 0) "[]"
  else print_injection state print_expr node.value

and print_set_mem state (node : set_mem reg) =
  print_loc_node state "SetMem" node.region;
  print_expr (state#pad 2 0) node.value.set;
  print_expr (state#pad 2 1) node.value.element

and print_set_injection state (node : expr injection reg) =
  print_loc_node  state "SetInj" node.region;
  print_injection state print_expr node.value

and print_some_app state (node : (kwd_Some * arguments) reg) =
  let _, args = node.value in
  print_loc_node   state "E_Some" node.region;
  print_tuple_expr state args

and print_E_Ctor state (node :  (ctor * arguments option) reg) =
  let ctor, args_opt = node.value in
  print_loc_node state "E_Ctor" node.region;
  let state = state#pad 1 0 in
  print_ident     state ctor;
  ignore args_opt (* TODO: remove. see below *)
(* TODO   print_option state print_tuple_expr args_opt*)

and print_op1 state label {value; region} =
  print_unary state label ~region print_expr value.arg

and print_op2 state label {value; region} =
  print_loc_node state label region;
  print_expr     (state#pad 2 0) value.arg1;
  print_expr     (state#pad 2 1) value.arg2

(* Printing tokens (client-slide) *)

type ('src, 'dst) printer = state -> 'src -> 'dst

let print_to_buffer state cst = print_cst state cst; state#buffer
let print_to_string state cst = print_to_buffer state cst |> Buffer.contents

(* Aliases *)

let to_buffer = print_to_buffer
let to_string = print_to_string
