(* A pretty printer for PascaLIGO *)

(* Disabled warnings *)

[@@@warning "-42-27-26"] (* TODO: Minimise *)

(* Vendor dependencies *)

(*module Directive = LexerLib.Directive*)
module Region = Simple_utils.Region
open! Region

(* Local dependencies *)

module CST = Cst_pascaligo.CST
open CST

(* Third party dependencies *)

open! PPrint


(* HIGHER-ORDER PRINTERS *)

let pp_par : ('a -> document) -> 'a par reg -> document =
  fun print {value; _} ->
    string "(" ^^ nest 1 (print value.inside ^^ string ")")

let pp_brackets : ('a -> document) -> 'a brackets reg -> document =
  fun print {value; _} ->
    string "[" ^^ nest 1 (print value.inside ^^ string "]")


(* PRINTING THE CST *)

let rec print cst =
  let decl = Utils.nseq_to_list cst.decl in
  print_declarations decl

(* DECLARATIONS (top-level) *)

and print_declarations (node : declarations) =
  let decl = List.filter_map print_declaration node
  in separate_map (hardline ^^ hardline) group decl

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_declaration = function
  D_Const     d -> Some (print_D_Const d)
| D_Directive _ -> None
| D_Fun       d -> Some (print_D_Fun d)
| D_Module    d -> Some (print_D_Module d)
| D_ModAlias  d -> Some (print_D_ModAlias d)
| D_Type      d -> Some (print_D_Type d)

(* Constant declaration *)

and print_D_const (node : const_decl reg) =
  let node       = node.value in
  let pattern    = node.pattern
  and const_type = node.const_type
  and init       = node.init
  and attributes = node.attributes
  in
  let thread     = string "const " ^^ print_pattern pattern in
  let thread     = print_attributes thread attributes in
  let thread     = print_const_type thread const_type in
  let thread     = print_init thread init
  in thread

and print_attribute (node : Attr.t reg) =
  let key, val_opt = attr.value in
  let thread = string "[@" ^^ string key in
  let thread = match val_opt with
                 Some value -> group (thread ^/^ nest 2 (string value))
               | None -> thread in
  let thread = thread ^^ string "]"
  in thread

and print_attributes thread = function
     [] -> thread
| attrs -> group (separate_map (break 0) print_attribute attrs ^/^ thread)

and print_type_annotation thread (_, type_expr : type_annotation) =
  group (thread ^/^ nest 2 (string ": " ^^ print_type_expr type_expr))

and print_init thread (node : expr) =
  thread ^^ group (break 1 ^^ nest 2 (string "= " ^^ print_expr node))

and print_const_type thread (node : type_annotation option) =
  match node with
    None -> thread
  | Some (_, e) ->
      group (thread ^/^ nest 2 (string ": " ^^ print_type_expr e))

(*
and print_dir_decl = function
  Directive.Linemarker {value; _} ->
    let open Directive in
    let linenum, file_path, flag_opt = value in
    let flag =
      match flag_opt with
        Some Push -> " 1"
      | Some Pop  -> " 2"
      | None      -> "" in
    let lexeme = Printf.sprintf "# %d %S%s" linenum file_path flag
    in string lexeme
*)

(* Function declaration *)

and print_D_Fun (node : fun_decl reg) =
  let node = node.value in
  let kwd_recursive = node.kwd_recursive
  and fun_name      = node.fun_name
  and param         = node.param
  and ret_type      = node.ret_type
  and return        = node.return
  and attributes    = node.attributes
  in
  let thread = print_kwd_recursive kwd_recursive in
  let thread = print_attributes thread attributes in
  let thread = group (thread ^/^ nest 2 (print_ident fun_name)) in
  let thread = group (thread ^//^ print_par print_parameters param) in
  let thread = thread ^^ print_ret_type ret_type in
  let thread = thread ^^ print_return return
  in thread

and print_return (node : expr) =
  let expr = print_expr node in
  match node with
    E_Block _ -> group (break 1 ^^ expr)
  | _         -> group (nest 2 (break 1 ^^ expr))

and print_ret_type (node : type_annotation option) =
  match node with
    None -> string " is"
  | Some (_, e) ->
      group (nest 2 (break 1 ^^ string ": " ^^ nest 2 (print_type_expr e)
                     ^^ string " is"))

and print_kwd_recursive (node : lexeme Wrap.t) =
  match kwd_recursive with
    None -> string "function"
  | Some _ -> string "recursive" ^/^ string "function"

and print_parameters p = print_nsepseq ";" print_param_decl p

and print_param_decl (node : param_decl) =
  let {param_kind; var; param_type} = node in
  match param_kind with
    `Var   _ -> print_param "var"   var param_type
  | `Const _ -> print_param "const" var param_type

and print_param thread var param_type =
  let name = string (thread ^ " ") ^^ print_variable var in
  match param_type with
    None -> name
  | Some (_, e) -> (name ^^ string " :") ^//^ print_type_expr e

and print_variable (node : variable) =
  let {variable; attributes} = value in
  let thread = print_ident node#payload in
  print_attributes thread node#attributes

(* Module declaration (structure) *)

and print_D_Module (node : module_decl reg) =
  let node = node.value in
  let name         = node.name
  and declarations = mode.declarations
  and enclosing    = node.enclosing
  in
  string "module " ^^ print_ident name ^^ string " is {"
  ^^ group (nest 2 (break 1 ^^ print_declarations declarations))
  ^^ string "}"

(* Declaration of module alias *)

and print_ModAlias (node : module_alias reg) =
  let node = node.value in
  let alias = node.alias
  and mod_path = node.mod_path in
  string "module " ^^ string alias.value
  ^^ group (nest 2 (break 1 ^^ print_nsepseq "." print_ident mod_path))

(* Type declaration *)

and print_D_Type (node : type_decl reg) =
  let node = node.value in
  let name = node.name
  and params = node.params
  and type_expr = node.type_expr
  in
  string "type " ^^ print_ident name
  ^^ print_type_params params ^^ string " is"
  ^^ group (nest 2 (break 1 ^^ print_type_expr type_expr))

and print_type_params (node : type_vars option) =
  match node with
    None -> empty
  | Some type_var ->
      let vars = print_nsepseq "," print_ident type_var.value.inside
      in string "(" ^^ nest 1 (vars ^^ string ")")

(* TYPE EXPRESSIONS *)

(* IMPORTANT: The data constructors are sorted alphabetically. If you
   add or modify some, please make sure they remain in order. *)

and print_type_expr = function
  T_App     t -> print_T_App     t
| T_Cart    t -> print_T_Cart    t
| T_Fun     t -> print_T_Fun     t
| T_Int     t -> print_T_Int     t
| T_ModPath t -> print_T_ModPath t
| T_Par     t -> print_T_Par     t
| T_Record  t -> print_T_Record  t
| T_String  t -> print_T_String  t
| T_Sum     t -> print_T_Sum     t
| T_Var     t -> print_T_Var     t

(* Application of type constructor *)

and print_T_App (node : (type_expr * type_tuple) reg) =
  let ctor_expr, tuple = node.value in
  print_type_expr ctor_expr ^//^ print_type_tuple tuple

and print_type_tuple (node : type_tuple) =
  let node = node.value in
  let head, tail = node.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_type_expr e)
  | e::items ->
      group (break 1 ^^ print_type_expr e ^^ string ",") ^^ app items in
  let components =
    if tail = []
    then print_type_expr head
    else print_type_expr head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

(* Cartesian type *)

and print_T_Cart (node : cartesian) =
  let head, tail = node.value in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_type_expr e)
  | e::items -> group (break 1 ^^ print_type_expr e ^^ string " *")
               ^^ app items
  in print_type_expr head ^^ string " *" ^^ app (Utils.nsepseq_to_list tail)

(* Functional type *)

and print_T_Fun {value; _} =
  let lhs, _, rhs = value in
  group (print_type_expr lhs ^^ string " ->" ^/^ print_type_expr rhs)

(* Integer type *)

and print_T_Int node = print_int node

(* Module paths *)

and print_T_ModPath (node : type_expr module_path reg) =
  print_module_path print_type_expr node

and print_module_path
  : type a.(a -> document) -> a module_path reg -> document =
  fun print node ->
    let node        = node.value in
    let module_path = node.module_path
    and field       = node.field in
    let modules     = Utils.nsepseq_to_list module_path
    and sep         = string "." ^^ break 0 in
    let modules     = separate_map sep print_ident fields in
    group (modules ^^ string "." ^^ break 0 ^^ print field)

(* XXX *)

and print_T_Var node = print_ident node

and print_T_String node = print_string node

and print_T_Sum {value; _} =
  let {variants; attributes; _} = value in
  let head, tail = variants in
  let head = print_variant head in
  let padding_flat =
    if attributes = [] then empty else string "| " in
  let padding_non_flat =
    if attributes = [] then blank 2 else string "| " in
  let head =
    if tail = [] then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head) in
  let rest = List.map snd tail in
  let app variant =
    group (break 1 ^^ string "| " ^^ print_variant variant) in
  let whole = head ^^ concat_map app rest in
  if attributes = [] then whole
  else group (print_attributes attributes ^/^ whole)

and print_variant {value; _} =
  let {ctor; args; attributes=attr} = value in
  let pre = if attr = [] then print_ident constr
            else group (print_attributes attr ^/^ print_ident ctor) in
  match args with
    None -> pre
  | Some (_,e) -> prefix 4 1 (pre ^^ string " of") (print_type_expr e)

and print_T_Record fields = group (print_compound print_field_decl fields)

and print_field_decl {value; _} =
  let {field_name; field_type; attributes; _} = value in
  let attr = print_attributes attributes in
  let name = if attributes = [] then print_ident field_name
             else attr ^/^ print_ident field_name in
  match field_type with
    None -> name
  | Some (_, type_expr) ->
     let t_expr = print_type_expr type_expr
     in prefix 2 1 (group (name ^^ string " :")) t_expr

and print_T_Par t = print_par print_type_expr t

(* Function and procedure declarations *)

and print_fun_expr {value; _} =
  let {param; ret_type; return; _} : fun_expr = value in
  let start      = string "function" in
  let parameters = print_par print_parameters param in
  let t_annot    =
    match ret_type with
      None -> empty
    | Some (_, e) ->
        group (break 1 ^^ nest 2 (string ": " ^^ print_type_expr e)) in
  group (start ^^ nest 2 (break 1 ^^ parameters))
  ^^ t_annot
  ^^ string " is" ^^ group (nest 4 (break 1 ^^ print_expr return))

(* Blocks *)

and print_block {value; _} =
  string "{"
  ^^ nest 2 (hardline ^^ print_statements value.statements)
  ^^ hardline ^^ string "}"

and print_statements s = print_nsepseq ";" print_statement s

and print_statement = function
  S_Instr   s -> print_instruction s
| S_Decl    s -> print_declaration s
| S_VarDecl s -> print_var_decl    s

and print_var_decl {value; _} =
  let {pattern; var_type; init; _} = value in
  let start = string "var " ^^ print_pattern pattern in
  let start =
    match var_type with
      None -> start
    | Some (_, e) ->
        group (start ^/^ nest 2 (string ": " ^^ print_type_expr e)) in
  start
  ^^ group (break 1 ^^ nest 2 (string ":= " ^^ print_expr init))

and print_instruction = function
  I_Assign i -> print_assignement i
| I_Call   i -> print_call i
| I_Case   i -> print_case print_test_clause i
| I_Cond   i -> print_conditional print_test_clause i
| I_For    i -> print_for_int i
| I_ForIn  of for_in reg CCC
| I_Patch  of patch reg
| I_Remove of removal reg
| I_Skip   of kwd_skip
| I_While  of while_loop reg

(*
  Cond        i -> group (print_conditional i)
| CaseInstr   i -> print_case print_if_clause i
| Assign      i -> print_assignment i
| Loop        i -> print_loop i
| ProcCall    i -> print_fun_call i
| Skip        _ -> string "skip"
| RecordPatch i -> print_record_patch i
| MapPatch    i -> print_map_patch i
| SetPatch    i -> print_set_patch i
| MapRemove   i -> print_map_remove i
| SetRemove   i -> print_set_remove i
 *)

and print_set_remove {value; _} =
  let {element; set; _} : set_remove = value in
  string "remove" ^^ group (nest 2 (break 1 ^^ print_expr element))
  ^^ group (break 1 ^^ prefix 2 1 (string "from set") (print_path set))

and print_map_remove {value; _} =
  let {key; map; _} = value in
  string "remove" ^^ group (nest 2 (break 1 ^^ print_expr key))
  ^^ group (break 1 ^^ prefix 2 1 (string "from map") (print_path map))

and print_set_patch {value; _} =
  let {path; set_inj; _} = value in
  let inj = print_ne_injection print_expr set_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_map_patch {value; _} =
  let {path; map_inj; _} = value in
  let inj = print_ne_injection print_binding map_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_binding {value; _} =
  let {source; image; _} = value in
  print_expr source
  ^^ string " ->" ^^ group (nest 2 (break 1 ^^ print_expr image))

and print_record_patch {value; _} =
  let {path; record_inj; _} = value in
  let inj = print_record record_inj in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ print_path path) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ inj))

and print_cond_expr {value; _} =
  let {test; ifso; ifnot; _} : cond_expr = value in
  let test  = string "if "  ^^ group (nest 3 (print_expr test))
  and ifso  = string "then" ^^ group (nest 2 (break 1 ^^ print_expr ifso))
  and ifnot = string "else" ^^ group (nest 2 (break 1 ^^ print_expr ifnot))
  in test ^/^ ifso ^/^ ifnot

and print_conditional {value; _} =
  let {test; ifso; ifnot; _} : conditional = value in
  let test  = string "if "  ^^ group (nest 3 (print_expr test))
  and ifso  = match ifso with
                ClauseInstr _ | ClauseBlock LongBlock _ ->
                  string "then"
                  ^^ group (nest 2 (break 1 ^^ print_if_clause ifso))
              | ClauseBlock ShortBlock _ ->
                  string "then {"
                  ^^ group (nest 2 (hardline ^^ print_if_clause ifso))
                  ^^ hardline ^^ string "}"
  and ifnot = match ifnot with
                ClauseInstr _ | ClauseBlock LongBlock _ ->
                  string "else"
                  ^^ group (nest 2 (break 1 ^^ print_if_clause ifnot))
              | ClauseBlock ShortBlock _ ->
                  string "else {"
                  ^^ group (nest 2 (hardline ^^ print_if_clause ifnot))
                  ^^ hardline ^^ string "}"
  in test ^/^ ifso ^/^ ifnot

and print_if_clause = function
  ClauseInstr i -> print_instruction i
| ClauseBlock b -> print_clause_block b

and print_clause_block = function
  LongBlock b  -> print_block b
| ShortBlock b -> Utils.(print_statements <@ fst) b.value.inside

and print_set_membership {value; _} =
  let {set; element; _} : set_membership = value in
  group (print_expr set ^/^ string "contains" ^/^ print_expr element)

and print_case : 'a.('a -> document) -> 'a case Region.reg -> document =
  fun printer {value; _} ->
    let {expr; cases; _} = value in
    group (string "case " ^^ nest 5 (print_expr expr) ^/^ string "of [")
    ^^ hardline ^^ print_cases printer cases
    ^^ hardline ^^ string "]"

and print_cases :
  'a.('a -> document) ->
    ('a case_clause reg, vbar) Utils.nsepseq Region.reg ->
    document =
  fun printer {value; _} ->
    let head, tail = value in
    let head       = print_case_clause printer head in
    let head       = blank 2 ^^ head in
    let rest       = List.map snd tail in
    let app clause = break 1 ^^ string "| " ^^ print_case_clause printer clause
    in  head ^^ concat_map app rest

and print_case_clause :
  'a.('a -> document) -> 'a case_clause Region.reg -> document =
  fun printer {value; _} ->
    let {pattern; rhs; _} = value in
    print_pattern pattern ^^ prefix 4 1 (string " ->") (printer rhs)

and print_assignment {value; _} =
  let {lhs; rhs; _} = value in
  prefix 2 1 (print_expr lhs ^^ string " :=") (print_expr rhs)

and print_loop = function
  While l -> print_while_loop l
| For f   -> print_for_loop f

and print_while_loop {value; _} =
  let {cond; block; _} = value in
  prefix 2 1 (string "while") (print_expr cond) ^^ hardline ^^ print_block block

and print_for_loop = function
  ForInt l     -> print_for_int l
| ForCollect l -> print_for_collect l

and print_for_int {value; _} =
  let {binder; init; bound; step; block; _} = value in
  let step =
    match step with
      None -> empty
    | Some (_, e) -> prefix 2 1 (string " step") (print_expr e) in
  prefix 2 1
         (string "for")
         (prefix 2 1 (print_ident binder ^^ string " :=") (print_expr init))
  ^^ prefix 2 1 (string " to") (print_expr bound)
  ^^ step ^^ hardline ^^ print_block block

and print_for_collect {value; _} =
  let {var; bind_to; collection; expr; block; _} = value in
  let binding =
    match bind_to with
      None -> print_ident var
    | Some (_, dest) -> print_ident var ^^ string " -> " ^^ print_ident dest in
  prefix 2 1 (string "for") binding
  ^^ prefix 2 1 (string " in") (print_collection collection ^/^ print_expr expr)
  ^^ hardline ^^ print_block block

and print_collection = function
  Map  _ -> string "map"
| Set  _ -> string "set"
| List _ -> string "list"

(* Expressions *)

and print_expr = function
  ECase       e -> print_case print_expr e
| ECond       e -> group (print_cond_expr e)
| EAnnot      e -> print_annot_expr e
| ELogic      e -> group (print_logic_expr e)
| EArith      e -> group (print_arith_expr e)
| EString     e -> print_string_expr e
| EList       e -> group (print_list_expr e)
| ESet        e -> print_set_expr e
| EConstr     e -> print_constr_expr e
| ERecord     e -> print_record e
| EProj       e -> print_projection e
| EModA       e -> print_module_path print_expr e
| EUpdate     e -> print_update e
| EMap        e -> print_map_expr e
| EVar        e -> print_ident e
| ECall       e -> print_fun_call e
| EBytes      e -> print_bytes e
| ETuple      e -> print_tuple_expr e
| EPar        e -> print_par print_expr e
| EFun        e -> print_fun_expr e
| ECodeInj    e -> print_code_inj e
| EBlock      e -> print_block_with e

and print_block_with {value; _} =
  let {block; kwd_with; expr} = value in
  let expr = value.expr in
  let expr = print_expr expr in
  group (print_block block ^^ string " with"
         ^^ group (nest 4 (break 1 ^^ expr)))

and print_annot_expr {value; _} =
  let expr, _, type_expr = value.inside in
  group (string "(" ^^ nest 1 (print_expr expr ^/^ string ": "
                               ^^ print_type_expr type_expr ^^ string ")"))

and print_set_expr = function
  SetInj inj -> print_injection print_expr inj
| SetMem mem -> print_set_membership mem

and print_map_expr = function
  MapLookUp fetch -> print_map_lookup fetch
| MapInj inj      -> print_injection print_binding inj
| BigMapInj inj   -> print_injection print_binding inj

and print_map_lookup {value; _} =
  prefix 2 1 (print_path value.path) (print_brackets print_expr value.index)

and print_path = function
  Name v -> print_ident v
| Path p -> print_projection p

and print_logic_expr = function
  BoolExpr e -> print_bool_expr e
| CompExpr e -> print_comp_expr e

and print_bool_expr = function
  Or   e  -> print_bin_op "or" e
| And  e  -> print_bin_op "and" e
| Not  e  -> print_un_op "not" e

and print_bin_op op {value; _} =
  let {arg1; arg2; _} = value
  and length = String.length op + 1 in
  print_expr arg1 ^/^ string (op ^ " ") ^^ nest length (print_expr arg2)

and print_un_op op {value; _} =
  string (op ^ " ") ^^ print_expr value.arg

and print_comp_expr = function
  Lt    e -> print_bin_op "<"  e
| Leq   e -> print_bin_op "<=" e
| Gt    e -> print_bin_op ">"  e
| Geq   e -> print_bin_op ">=" e
| Equal e -> print_bin_op "="  e
| Neq   e -> print_bin_op "=/=" e

and print_arith_expr = function
  Add   e -> print_bin_op "+" e
| Sub   e -> print_bin_op "-" e
| Mult  e -> print_bin_op "*" e
| Div   e -> print_bin_op "/" e
| Mod   e -> print_bin_op "mod" e
| Neg   e -> string "-" ^^ print_expr e.value.arg
| Int   e -> print_int e
| Nat   e -> print_nat e
| Mutez e -> print_mutez e

and print_mutez {value; _} =
  Z.to_string (snd value) ^ "mutez" |> string

and print_string_expr = function
  Cat      e -> print_bin_op "^" e
| String   e -> print_string e
| Verbatim e -> print_verbatim e

and print_ident {value; _} = string value

and print_string s = string "\"" ^^ print_ident s ^^ string "\""

and print_verbatim s = string "{|" ^^ print_ident s ^^ string "|}"

and print_list_expr = function
  ECons     e -> print_bin_op "#" e
| EListComp e -> print_injection print_expr e
| ENil      _ -> string "nil"

and print_constr_expr {value; _} =
  let constr, args = value in
  let constr = string constr.value in
  match args with
          None -> constr
  | Some tuple -> prefix 2 1 constr (print_tuple_expr tuple)

and print_field_assign {value; _} =
  let {field_name; field_expr; _} = value in
  prefix 2 1 (print_ident field_name ^^ string " =") (print_expr field_expr)

and print_record ne_inj = group (print_ne_injection print_field_assign ne_inj)

and print_projection {value; _} =
  let {struct_name; field_path; _} = value in
  let fields = Utils.nsepseq_to_list field_path
  and sep    = string "." ^^ break 0 in
  let fields = separate_map sep print_selection fields in
  group (print_ident struct_name ^^ string "." ^^ break 0 ^^ fields)

and print_update {value; _} =
  let {record; updates; _} = value in
  let updates = group (print_ne_injection print_field_path_assign updates)
  and record  = print_path record in
  record ^^ string " with" ^^ nest 2 (break 1 ^^ updates)

and print_code_inj {value; _} =
  let {language; code; _} = value in
  let language = string language.value.value
  and code     = print_expr code in
  string "[%" ^^ language ^/^ code ^^ string "]"

and print_field_path_assign {value; _} =
  let {field_path; field_expr; _} = value in
  let path = print_path field_path in
  prefix 2 1 (path ^^ string " =") (print_expr field_expr)

and print_selection = function
  FieldName v   -> string v.value
| Component cmp -> cmp.value |> snd |> Z.to_string |> string

and print_tuple_expr {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_expr e)
  | e::items ->
      group (break 1 ^^ print_expr e ^^ string ",") ^^ app items in
  let components =
    if tail = []
    then print_expr head
    else print_expr head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

and print_fun_call {value; _} =
  let lambda, arguments = value in
  let arguments = print_tuple_expr arguments in
  group (print_expr lambda ^^ nest 2 (break 1 ^^ arguments))

(* Injections *)

and print_injection :
  'a.('a -> document) -> 'a injection reg -> document =
  fun printer {value; _} ->
    let {kind; elements; _} = value in
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep printer elements in
    let kwd      = print_injection_kwd kind in
    group (string (kwd ^ " [")
           ^^ nest 2 (break 0 ^^ elements) ^^ break 0 ^^ string "]")

and print_injection_kwd = function
  InjSet    _ -> "set"
| InjMap    _ -> "map"
| InjBigMap _ -> "big_map"
| InjList   _ -> "list"
| InjRecord _ -> "record"

and print_ne_injection :
  'a.('a -> document) -> 'a ne_injection reg -> document =
  fun printer {value; _} ->
    let {kind; ne_elements; attributes; _} = value in
    let elements = print_nsepseq ";" printer ne_elements in
    let kwd      = print_ne_injection_kwd kind in
    let inj      = group (string (kwd ^ " [")
                          ^^ group (nest 2 (break 0 ^^ elements ))
                          ^^ break 0 ^^ string "]") in
    let inj      = if attributes = [] then inj
                   else group (print_attributes attributes ^/^ inj)
    in inj

and print_ne_injection_kwd = function
  NEInjSet    _ -> "set"
| NEInjMap    _ -> "map"
| NEInjRecord _ -> "record"

and print_nsepseq :
  'a.string -> ('a -> document) -> ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun sep printer elements ->
    let elems = Utils.nsepseq_to_list elements
    and sep   = string sep ^^ break 1
    in separate_map sep printer elems

(* Patterns *)

and print_pattern = function
  PConstr p -> print_constr_pattern p
| PVar    v -> print_variable v
| PInt    i -> print_int i
| PNat    n -> print_nat n
| PBytes  b -> print_bytes b
| PString s -> print_string s
| PList   l -> print_list_pattern l
| PTuple  t -> print_tuple_pattern t
| PRecord r -> print_record_pattern r

and print_record_pattern fields = print_injection print_field_pattern fields

and print_field_pattern {value; _} =
  let {field_name; pattern; _} = value in
  prefix 2 1 (print_ident field_name ^^ string " =") (print_pattern pattern)

and print_int {value; _} =
  string (Z.to_string (snd value))

and print_nat {value; _} =
  string (Z.to_string (snd value) ^ "n")

and print_bytes {value; _} =
  string ("0x" ^ Hex.show (snd value))

and print_constr_pattern {value; _} =
  match value with
    constr, None -> print_ident constr
  | constr, Some ptuple ->
      prefix 4 1 (print_ident constr) (print_tuple_pattern ptuple)

and print_tuple_pattern {value; _} =
  let head, tail = value.inside in
  let rec app = function
    []  -> empty
  | [e] -> group (break 1 ^^ print_pattern e)
  | e::items ->
      group (break 1 ^^ print_pattern e ^^ string ",") ^^ app items in
  let components =
    if   tail = []
    then print_pattern head
    else print_pattern head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

and print_list_pattern = function
  PListComp cmp -> print_list_comp cmp
| PNil _        -> string "nil"
| PParCons p    -> print_ppar_cons p
| PCons p       -> nest 4 (print_nsepseq " #" print_pattern p.value)

and print_list_comp e = print_injection print_pattern e

and print_ppar_cons {value; _} =
  let patt1, _, patt2 = value.inside in
  let comp = prefix 2 1 (print_pattern patt1 ^^ string " ::") (print_pattern patt2)
  in string "(" ^^ nest 1 (comp ^^ string ")")

let print_type_expr = print_type_expr
let print_pattern   = print_pattern
let print_expr      = print_expr

type cst        = CST.t
type expr       = CST.expr
type type_expr  = CST.type_expr
type pattern    = CST.pattern
