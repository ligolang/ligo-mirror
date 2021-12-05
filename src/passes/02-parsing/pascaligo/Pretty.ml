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

  and print_return (node : expr) =
    let expr = print_expr node in
    match node with
      E_Block _ -> break 1 ^^ expr
    | _         -> nest 2 (break 1 ^^ expr)

  and print_ret_type (node : type_annotation option) =
    match node with
      None -> string " is"
    | Some (_, e) ->
        let e = nest 2 (print_type_expr e)
        in nest 2 (break 1 ^^ string ": " ^^ e ^^ string " is")

  and print_kwd_recursive (node : lexeme Wrap.t) =
    if kwd_recursive = None then
      string "function"
    else string "recursive" ^/^ string "function"
  in
  let thread = group (print_kwd_recursive kwd_recursive) in
  let thread = print_attributes thread attributes in
  let thread = group (thread ^/^ nest 2 (print_ident fun_name)) in
  let thread = group (thread ^//^ print_par print_parameters param) in
  let thread = thread ^^ group (print_ret_type ret_type) in
  let thread = thread ^^ group (print_return return)
  in thread

and print_parameters p = print_nsepseq ";" print_param_decl p

and print_param_decl (node : param_decl) =
  let {param_kind; var; param_type} = node in
  match param_kind with
    `Var   _ -> print_param "var"   var param_type
  | `Const _ -> print_param "const" var param_type

and print_param thread var param_type =
  let thread = string (thread ^ " ") ^^ print_variable var in
  match param_type with
    None -> thread
  | Some (_, e) -> (thread ^^ string " :") ^//^ print_type_expr e

and print_variable (node : variable) =
  let {variable; attributes} = node.value in
  let thread = print_ident node#payload in
  print_attributes thread node#attributes

(* Module declaration (structure) *)

and print_D_Module (node : module_decl reg) =
  let node         = node.value in
  let name         = node.name
  and declarations = mode.declarations
  in
  let declarations = nest 2 (break 1 ^^ print_declarations declarations)
  in string "module " ^^ print_ident name ^^ string " is {"
     ^^ group declarations ^^ string "}"

(* Declaration of module alias *)

and print_ModAlias (node : module_alias reg) =
  let node     = node.value in
  let alias    = node.alias
  and mod_path = node.mod_path
  in
  let mod_path = print_nsepseq "." print_ident mod_path
  in string "module " ^^ string alias.value
     ^^ group (nest 2 (break 1 ^^ mod_path))

(* Type declaration *)

and print_D_Type (node : type_decl reg) =
  let node      = node.value in
  let name      = node.name
  and params    = node.params
  and type_expr = node.type_expr
  in
  let type_expr = nest 2 (break 1 ^^ print_type_expr type_expr)
  in string "type " ^^ print_ident name
     ^^ print_type_params params ^^ string " is" ^^ group type_expr

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
  let head = print_type_expr head
  and app = function
    []       -> empty
  | [e]      -> group (break 1 ^^ print_type_expr e)
  | e::items -> group (break 1 ^^ print_type_expr e ^^ string ",")
               ^^ app items in
  let components =
    if tail = [] then head
    else head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

(* Cartesian type *)

and print_T_Cart (node : cartesian) =
  let head, _, tail = node.value in
  let head = print_type_expr head
  and app = function
    []       -> empty
  | [e]      -> group (break 1 ^^ print_type_expr e)
  | e::items -> group (break 1 ^^ print_type_expr e ^^ string " *")
                ^^ app items
  in head ^^ string " *" ^^ app (Utils.nsepseq_to_list tail)

(* Functional type *)

and print_T_Fun (node : (type_expr * arrow * type_expr) reg) =
  let lhs, _, rhs = node.value in
  let lhs         = print_type_expr lhs
  and rhs         = print_type_expr rhs
  in group ( lhs ^^ string " ->" ^/^ rhs)

(* Integer type *)

and print_T_Int (node :  (lexeme * Z.t) wrap) = print_int node

(* Module path *)

and print_T_ModPath (node : type_expr module_path reg) =
  print_module_path print_type_expr node

and print_module_path
  : type a.(a -> document) -> a module_path reg -> document =
  fun print node ->
    let node        = node.value in
    let module_path = node.module_path
    and field       = node.field
    in
    let modules     = Utils.nsepseq_to_list module_path
    and sep         = string "." ^^ break 0 in
    let modules     = separate_map sep print_ident fields in
    group (modules ^^ sep ^^ print field)

(* Type variable *)

and print_T_Var (node : variable) = print_ident node

(* Type string *)

and print_T_String (node : lexeme wrap) = print_string node

(* Sum type *)

and print_T_Sum (node : sum_type reg) =
  let node       = node.value in
  let head, tail = node.variants
  and attributes = node.attributes
  in
  let head = print_variant head
  and padding_flat =
    if attributes = [] then empty else string "| "
  and padding_non_flat =
    if attributes = [] then blank 2 else string "| " in
  let head =
    if tail = [] then head
    else ifflat (padding_flat ^^ head) (padding_non_flat ^^ head)
  and tail = List.map snd tail
  and app variant =
    group (break 1 ^^ string "| " ^^ print_variant variant) in
  let thread = head ^^ concat_map app tail
  in print_attributes thread attributes

and print_variant (node : variant reg) =
  let node       = node.value in
  let ctor       = node.ctor
  and args       = node.args
  and attributes = node.attributes
  in
  let thread     = print_ident ctor in
  let thread     = print_attributes thread attributes in
  match args with
    None -> thread
  | Some (_,e) -> let args = print_type_expr e
                 in prefix 4 1 (thread ^^ string " of") args

(* Record type *)

and print_T_Record (node : field_decl reg compound reg) =
  group (print_compound print_field_decl node)

and print_field_decl (node : field_decl reg) =
  let node       = node.value in
  let field_name = node.field_name
  and field_type = node.field_type
  and attributes = node.attributes
  in
  let thread     = print_ident field_name in
  let thread     = print_attributes thread attributes in
  match field_type with
    None -> thread
  | Some (_, e) -> (thread ^^ string " :") ^//^ print_type_expr e

(* Parenthesised type *)

and print_T_Par (node : type_expr par reg) =
  print_par print_type_expr node

(* Function and procedure declarations *)

and print_fun_expr (node : fun_expr reg) =
  let node       = node.value in
  let parameters = node.parameters
  and ret_type   = node.ret_type
  and ret_expr   = node.return
  in
  let thread     = string "function" in
  let parameters = print_par print_parameters parameters in
  let thread     = group (thread ^^ nest 2 (break 1 ^^ parameters)) in
  let thread     =
    match ret_type with
      None -> thread
    | Some (_, e) ->
       let e = print_type_expr e
       in thread ^^ group (break 1 ^^ nest 2 (string ": " ^^ e)) in
  let ret_expr = nest 4 (break 1 ^^ print_expr ret_expr) in
  let thread = thread ^^ string " is" ^^ group ret_expr
  in thread

(* Blocks *)

and print_block (node : block reg) =
  let statements = node.value.statements in
  string "{"
  ^^ nest 2 (hardline ^^ print_statements statements) ^^ hardline
  ^^ string "}"

(* STATEMENTS *)

and print_statements (node : statements) =
  print_nsepseq ";" print_statement node

and print_statement (node : statement) =
  match node with
    S_Instr   s -> print_S_Instr   s
  | S_Decl    s -> print_S_Decl    s
  | S_VarDecl s -> print_S_VarDecl s


(* INSTRUCTIONS *)

and print_S_Instr (node : instruction) =
  match node with
    I_Assign i -> print_I_Assign i
  | I_Call   i -> print_I_Call   i
  | I_Case   i -> print_I_Case   i
  | I_Cond   i -> print_I_Cond   i
  | I_For    i -> print_I_For    i
  | I_ForIn  i -> print_I_ForIn  i
  | I_Patch  i -> print_I_Patch  i
  | I_Remove i -> print_I_Remove i
  | I_Skip   i -> print_I_Skip   i
  | I_While  i -> print_I_While  i

(* Assignment *)

and print_I_Assign (node : assignment reg) =
  let node = node.value in
  let lhs  = node.lhs
  and rhs  = node.rhs
  in print_expr lhs ^^ string " :=" ^//^ print_expr rhs

(* Procedure call *)

and print_I_Call (node : call) =
  let node              = node.value in
  let lambda, arguments = node in
  let arguments         = print_tuple_expr arguments in
  group (print_expr lambda ^^ nest 2 (break 1 ^^ arguments))

(* Case *)

and print_I_Case (node : test_clause case reg) =
  print_case print_test_clause node

and print_case : 'a.('a -> document) -> 'a case Region.reg -> document =
  fun printer node ->
    let node  = node.value in
    let expr  = node.expr
    and cases = node.case in
    let expr = nest 5 (print_expr expr)
    in
    group (string "case " ^^ expr ^/^ string "of [")
    ^^ hardline ^^ print_cases printer cases
    ^^ hardline ^^ string "]"

and print_cases :
  'a.('a -> document) ->
    ('a case_clause reg, vbar) Utils.nsepseq Region.reg ->
    document =
  fun printer node ->
    let node       = node.value in
    let head, tail = node in
    let head       = print_case_clause printer head in
    let head       = blank 2 ^^ head in
    let tail       = List.map snd tail in
    let app clause = break 1 ^^ string "| "
                     ^^ print_case_clause printer clause
    in head ^^ concat_map app tail

and print_case_clause :
  'a.('a -> document) -> 'a case_clause Region.reg -> document =
  fun printer node ->
    let node    = node.value in
    let pattern = node.pattern
    and rhs     = node.rhs in
    print_pattern pattern ^^ prefix 4 1 (string " ->") (printer rhs)

(* Conditional instruction *)

and print_I_Cond (node : test_clause conditional reg) =
  let node   = node.value in
  let test   = node.test
  and if_so  = node.if_so
  and if_not = node.if_not
  in
  let thread = string "if " ^^ group (nest 3 (print_expr test))
  and thread = thread ^/^ string "then"
               ^^ match if_so with
                    ClauseInstr _ ->
                      group (nest 2 (break 1 ^^ print_test_clause if_so))
                  | ClauseBlock _ ->
                      string " {"
                      ^^ group (nest 2 (hardline ^^ print_test_clause if_so))
                      ^^ hardline ^^ string "}"
  and thread = match if_not with
                Some ClauseInstr _ ->
                  thread ^/^ string "else"
                  ^^ group (nest 2 (break 1 ^^ print_test_clause if_not))
              | Some ClauseBlock _ ->
                  thread ^/^ string "else {"
                  ^^ group (nest 2 (hardline ^^ print_test_clause if_not))
                  ^^ hardline ^^ string "}"
              | None -> thread
  in group thread

and print_test_clause = function
  ClauseInstr i -> print_ClauseInstr i
| ClauseBlock i -> print_ClauseBlock i

and print_ClauseInstr (node : instruction) = print_I_Instr node

and print_ClauseBlock (node : block reg) = print_block node

(*
and print_clause_block = function
  LongBlock b  -> print_block b
| ShortBlock b -> Utils.(print_statements <@ fst) b.value.inside
 *)

(* XXX *)

and print_var_decl {value; _} =
  let {pattern; var_type; init; _} = value in
  let thread = string "var " ^^ print_pattern pattern in
  let thread =
    match var_type with
      None -> thread
    | Some (_, e) ->
        let annotation = nest 2 (string ": " ^^ print_type_expr e)
        in group (thread ^/^ annotation) in
  let init = print_expr init in
  let assignment = group (break 1 ^^ nest 2 (string ":= " ^^ init))
  in thread  ^^ assignment

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
  let {test; if_so; if_not; _} : cond_expr = value in
  let test  = string "if "  ^^ group (nest 3 (print_expr test))
  and if_so  = string "then" ^^ group (nest 2 (break 1 ^^ print_expr if_so))
  and if_not = string "else" ^^ group (nest 2 (break 1 ^^ print_expr if_not))
  in test ^/^ if_so ^/^ if_not


and print_set_membership {value; _} =
  let {set; element; _} : set_membership = value in
  group (print_expr set ^/^ string "contains" ^/^ print_expr element)

and print_loop = function
  While l -> print_while_loop l
| For f   -> print_for_loop f

and print_while_loop {value; _} =
  let {cond; block; _} = value in
  prefix 2 1 (string "while") (print_expr cond) ^^ hardline ^^ print_block block

and print_for_loop = function
  ForInt l     -> print_for_int l
| ForCollect l -> print_for_collect l

and print_I_For {value; _} =
  let {binder; init; bound; step; block; _} = value in
  let step =
    match step with
      None -> empty
    | Some (_, e) -> string " step" ^//^ print_expr e
  in string "for"
     ^//^ (print_ident binder ^^ string " :=") ^//^ print_expr init
     ^^ (string " to" ^//^ print_expr bound)
     ^^ step ^^ hardline ^^ print_block block

and print_I_ForIn {value; _} =
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
