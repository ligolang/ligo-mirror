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

(* PRINTING LITERALS *)

let print_bytes (node : (lexeme * Hex.t) wrap) =
  string ("0x" ^ Hex.show (snd node#payload))

let print_mutez (node : (lexeme * Int64.t) wrap) =
  Int64.to_string (snd node#payload) ^ "mutez" |> string

let print_ident (node : variable) = string node#payload

let print_string (node : lexeme wrap) =
  string "\"" ^^ print_ident node ^^ string "\""

let print_verbatim (node : lexeme wrap) =
  string "{|" ^^ print_ident node ^^ string "|}"

let print_int (node : (lexeme * Z.t) wrap) =
  string (Z.to_string (snd node#payload))

and print_nat (node : (lexeme * Z.t) wrap) =
  string (Z.to_string (snd node#payload) ^ "n")


(* HIGHER-ORDER PRINTERS *)

let print_par : ('a -> document) -> 'a par reg -> document =
  fun print {value; _} ->
    string "(" ^^ nest 1 (print value.inside ^^ string ")")

let print_brackets : ('a -> document) -> 'a brackets reg -> document =
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
  let thread     = print_opt_type thread const_type in
  let thread     = print_init "= " thread init
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

and print_init op thread (node : expr) =
  thread ^^ group (break 1 ^^ nest 2 (string op ^^ print_expr node))

and print_opt_type thread (node : type_annotation option) =
  match node with
    None -> thread
  | Some (_, e) -> print_type_annotation thread e

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

and print_parameters p = print_nsepseq print_param_decl p

and print_nsepseq :
  'a.('a -> document) -> ('a, lexeme Wrap.t) Utils.nsepseq -> document =
  fun print ->
    function
      head, [] -> print head
    | _, (sep, _)::_ as elements ->
        let elems = Utils.nsepseq_to_list elements
        and sep   = string sep ^^ break 1
        in separate_map sep print elems

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
  let mod_path = print_nsepseq print_ident mod_path
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
      let vars = print_nsepseq print_ident type_var.value.inside
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

and print_compound : 'a.('a -> document) -> 'a compound reg -> document =
  fun print node ->
    let node       = node.value in
    let kind       = node.kind
    and elements   = node.elements
    and attributes = node.attributes
    in
    let sep      = string ";" ^^ break 1 in
    let elements = Utils.sepseq_to_list elements in
    let elements = separate_map sep print elements in
    group (string (kind#payload ^ " [")
           ^^ nest 2 (break 0 ^^ elements) ^^ break 0 ^^ string "]")

(* Parenthesised type *)

and print_T_Par (node : type_expr par reg) =
  print_par print_type_expr node

(* Blocks *)

and print_block (node : block reg) =
  let statements = node.value.statements in
  string "{"
  ^^ nest 2 (hardline ^^ print_statements statements) ^^ hardline
  ^^ string "}"

(* STATEMENTS *)

and print_statements (node : statements) =
  print_nsepseq print_statement node

and print_statement (node : statement) =
  match node with
    S_Instr   s -> print_S_Instr   s
  | S_Decl    s -> print_S_Decl    s
  | S_VarDecl s -> print_S_VarDecl s

(* Variable declaration (invalid at the top-level) *)

and print_var_decl (node : var_decl reg) =
  let node       = node.value in
  let pattern    = node.pattern
  and var_type   = node.var_type
  and init       = node.init
  and attributes = node.attributes
  in
  let thread     = string "var " ^^ print_pattern pattern in
  let thread     = print_attributes thread attributes in
  let thread     = print_opt_type thread var_type in
  let thread     = print_init ":= " thread init
  in group thread

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
  in group (print_expr lhs ^^ string " :=" ^//^ print_expr rhs)

(* Procedure call *)

and print_I_Call (node : call) = print_call node

and print_call (node : call) =
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
    in group (head ^^ concat_map app tail)

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

(* Interation over integer intervals *)

and print_I_For (node : for_int reg) =
  let node  = node.value in
  let index = node.index
  and init  = node.init
  and bound = node.bound
  and step  = node.step
  and block = node.block
  in
  let first = (print_ident index ^^ string " :=") ^//^ print_expr init
  and last = string " to" ^//^ print_expr bound
  and step = match step with
               None -> empty
             | Some (_, e) -> string " step" ^//^ print_expr e
  in group (string "for" ^//^ first ^^ last ^^ step ^^ hardline
            ^^ print_block block)

(* Iteration over collections *)

and print_I_ForIn (node : for_in reg) =
  let node       = node.value in
  let var        = node.var
  and bind_to    = node.bind_to
  and collection = node.collection
  and block      = node.block
  in
  let src     = print_ident var in
  let binding = match bind_to with
                  None -> src
                | Some (_, dest) ->
                    src ^^ string " -> " ^^ print_ident dest
  in group ((string "for" ^//^ binding)
            ^^ (string " in" ^//^ print_expr collection)
            ^^ hardline ^^ print_block block)

(* Patches for maps, records, and sets. *)

and print_I_Patch (node : patch reg) =
  let node       = node.value in
  let collection = node.collection
  and patch      = node.expr
  in
  let collection = print_expr collection
  and patch      = print_expr patch
  in
  string "patch"
  ^^ group (nest 2 (break 1 ^^ collection) ^/^ string "with")
  ^^ group (nest 2 (break 1 ^^ patch))

(* Removal from sets and maps *)

and print_I_Remove (node : removal reg) =
  let node       = node.value in
  let item       = node.item
  and collection = node.collection
  in
  let item       = print_expr item
  and collection = print_expr collection
  in
  string "remove" ^^ group (nest 2 (break 1 ^^ item))
  ^^ group (break 1 ^^ string "from" ^//^ collection)

(* Skipping *)

and print_I_Skip (node : kwd_skip) = string node#payload

(* General loop *)

and print_I_While (node : while_loop reg) =
  let node = node.value in
  let cond = node.cond
  and block = node.block
  in group ((string "while" ^//^ print_expr cond)
            ^^ hardline ^^ print_block block)


(* PATTERNS *)

and print_pattern (node : pattern) =
  match pattern with
    P_App     p -> print_P_App     p
  | P_Bytes   p -> print_P_Bytes   p
  | P_Cons    p -> print_P_Cons    p
  | P_Ctor    p -> print_P_Ctor    p
  | P_Int     p -> print_P_Int     p
  | P_List    p -> print_P_List    p
  | P_ModPath p -> print_P_ModPath p
  | P_Mutez   p -> print_P_Mutez   p
  | P_Nat     p -> print_P_Nat     p
  | P_Nil     p -> print_P_Nil     p
  | P_Par     p -> print_P_Par     p
  | P_Record  p -> print_P_Record  p
  | P_String  p -> print_P_String  p
  | P_Tuple   p -> print_P_Tuple   p
  | P_Typed   p -> print_P_Typed   p
  | P_Var     p -> print_P_Var     p

(* Pattern for the application of a data constructor *)

and print_P_App (node : (pattern * tuple_pattern option) reg) =
  match node.value with
    ctor, None -> print_pattern ctor
  | ctor, Some tuple ->
      group (print_pattern ctor ^//^ print_tuple print_pattern tuple)

and print_tuple :
  'a.('a -> document) -> ('a, comma) Utils.nsepseq par reg -> document =
  fun print node ->
    let node = node.value in
    let head, tail = node.inside in
    let rec app = function
        []       -> empty
      | [x]      -> group (break 1 ^^ print x)
      | x::items -> group (break 1 ^^ print x ^^ string ",") ^^ app items in
    let components =
      if   tail = []
      then print head
      else print head ^^ string "," ^^ app (List.map snd tail)
    in string "(" ^^ nest 1 (components ^^ string ")")

and print_tuple_pattern (node : tuple_pattern) =
  let node = node.value in
  let head, tail = node.inside in
  let rec app = function
    []       -> empty
  | [p]      -> group (break 1 ^^ print_pattern p)
  | p::items -> group (break 1 ^^ print_pattern p ^^ string ",")
               ^^ app items in
  let components =
    if   tail = []
    then print_pattern head
    else print_pattern head ^^ string "," ^^ app (List.map snd tail)
  in string "(" ^^ nest 1 (components ^^ string ")")

(* Pattern bytes *)

and print_P_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Pattern for consing *)

and print_P_Cons (node : (pattern * sharp * pattern) reg) =
  let node = node.value in
  let head, _, tail = node
  in group ((print_pattern head ^^ string " #") ^//^ print_pattern tail)

(* Constructor in a pattern *)

and print_P_Ctor (node : ctor) = print_ident node#payload

(* Integer in a pattern *)

and print_P_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lists *)

and print_P_List (node : pattern compound reg) =
  print_compound print_pattern node

(* Module paths in patterns *)

and print_P_ModPath (node : pattern module_path reg) =
  print_module_path print_pattern node

(* Mutez in patterns *)

and print_P_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Natural numbers in patterns *)

and print_P_Nat (node : (lexeme * Z.t) wrap) = print_nat node

(* The empty list in patterns *)

and print_P_Nil (node : kwd_nil) = string node#payload

(* Parenthesised pattern *)

and print_P_Par (node : pattern par reg) =
  print_par print_pattern node

(* Record pattern *)

and print_P_Record (node : record_pattern) =
  print_compound print_field_pattern node

and print_field_pattern (node : field_pattern reg) =
  match node.value with
    Punned {pun; attributes} ->
      let thread = print_ident pun in
      let thread = print_attributes thread attributes
      in thread
  | Complete {field_lhs; field_rhs; attributes; _} ->
      let thread = group ((print_ident field_lhs ^^ string " =")
                          ^//^ print_pattern field_rhs) in
      let thread = print_attributes thread attributes
      in thread

(* String patterns *)

and print_P_String (node : lexeme wrap) = print_string node

(* Tuple patterns *)

and print_P_Tuple (node : tuple_pattern) =
  print_par (print_nsepseq print_pattern) node.value

(* Typed patterns *)

and print_P_Typed (node : typed_pattern reg) =
  let {pattern; type_annot} = node.value in
  let _, type_expr = type_annot in
  group ((print_pattern pattern ^^ ":") ^//^ print_type_expr type_expr)

(* Variable pattern *)

and print_P_Var (node : variable) = print_variable node

(* EXPRESSIONS *)

and print_expr (node : expr) =
  match node with
    E_Add       e -> print_E_Add       e
  | E_App       e -> print_E_App       e
  | E_And       e -> print_E_And       e
  | E_BigMap    e -> print_E_BigMap    e
  | E_Block     e -> print_E_Block     e
  | E_Bytes     e -> print_E_Bytes     e
  | E_Call      e -> print_E_Call      e
  | E_Case      e -> print_E_Case      e
  | E_Cat       e -> print_E_Cat       e
  | E_CodeInj   e -> print_E_CodeInj   e
  | E_Ctor      e -> print_E_Ctor      e
  | E_Equal     e -> print_E_Equal     e
  | E_Cond      e -> print_E_Cond      e
  | E_Cons      e -> print_E_Cons      e
  | E_Div       e -> print_E_Div       e
  | E_Fun       e -> print_E_Fun       e
  | E_Geq       e -> print_E_Geq       e
  | E_Gt        e -> print_E_Gt        e
  | E_Int       e -> print_E_Int       e
  | E_Leq       e -> print_E_Leq       e
  | E_List      e -> print_E_List      e
  | E_Lt        e -> print_E_Lt        e
  | E_Map       e -> print_E_Map       e
  | E_MapLookup e -> print_E_MapLookup e
  | E_Mod       e -> print_E_Mod       e
  | E_Mult      e -> print_E_Mult      e
  | E_Mutez     e -> print_E_Mutez     e
  | E_Nat       e -> print_E_Nat       e
  | E_Neg       e -> print_E_Neg       e
  | E_Nil       e -> print_E_Nil       e
  | E_Neq       e -> print_E_Neq       e
  | E_Not       e -> print_E_Not       e
  | E_Or        e -> print_E_Or        e
  | E_Par       e -> print_E_Par       e
  | E_Record    e -> print_E_Record    e
  | E_Set       e -> print_E_Set       e
  | E_SetMem    e -> print_E_SetMem    e
  | E_String    e -> print_E_String    e
  | E_Sub       e -> print_E_Sub       e
  | E_Tuple     e -> print_E_Tuple     e
  | E_Typed     e -> print_E_Typed     e
  | E_Update    e -> print_E_Update    e
  | E_Verbatim  e -> print_E_Verbatim  e
  | E_ModPath   e -> print_E_ModPath   e
  | E_Var       e -> print_E_Var       e
  | E_Proj      e -> print_E_Proj      e

(* Addition *)

and print_E_Add (node : plus bin_op reg) = print_bin_op node

and print_bin_op (node : lexeme wrap bin_op reg) =
  let node = node.value in
  let {op; arg1; arg2} = node in
  let length = String.length op#payload + 1
  in group (print_expr arg1 ^/^ string (op#payload ^ " ")
            ^^ nest length (print_expr arg2))

(* Application to data constructors *)

and print_E_App (node : (expr * arguments option) reg) =
  match node.value with
    ctor, None -> print_expr ctor
  | ctor, Some tuple ->
      group (print_expr ctor ^//^ print_tuple print_expr tuple)

(* Logical conjunction *)

and print_E_And (node : kwd_and bin_op reg) = print_bin_op node

(* Big map expression *)

and print_E_BigMap (node : binding reg compound reg) =
  print_compound print_binding node

and print_binding (node : binding reg) =
  let node  = node.value in
  let key   = print_expr node.key
  and value = print_expr node.value
  in key ^^ string " ->" ^^ group (nest 2 (break 1 ^^ value))

(* Block expression *)

and print_E_Block (node : block_with reg) =
  let node  = node.value in
  let block = node.block
  and expr  = node.expr
  in
  let expr = print_expr expr in
  group (print_block block ^^ string " with"
         ^^ group (nest 4 (break 1 ^^ expr)))

(* Bytes expressions *)

and print_E_Bytes (node : (lexeme * Hex.t) wrap) = print_bytes node

(* Function calls *)

and print_E_Call (node : call) = print_call node

(* Case expressions *)

and print_E_Case (node : expr case reg) =
  print_case print_expr node

(* String concatenation *)

and print_E_Cat (node : caret bin_op reg) = print_bin_op node

(* Code injection *)

and print_E_CodeInj (node : code_inj reg) =
  let node     = node.value in
  let language = node.language
  and code     = node.code
  in
  let language = string language.value.value
  and code     = print_expr code in
  group (string "[%" ^^ language ^/^ code ^^ string "]")

(* Constructor in expressions *)

and print_E_Ctor (node : ctor) = print_ident node#payload

(* Equality *)

and print_E_Equal (node : equal bin_op reg) = print_bin_op node

(* Conditional expression *)

and print_E_Cond (node : expr conditional reg) =
  let node   = node.value in
  let test   = node.test
  and if_so  = node.if_so
  and if_not = node.if_not
  in
  let thread = string "if "  ^^ group (nest 3 (print_expr test))
  and if_so  = thread ^/^ string "then"
               ^^ group (nest 2 (break 1 ^^ print_expr if_so))
  and if_not =
    match if_not with
      None -> thread
    | Some e -> thread ^/^ string "else"
               ^^ group (nest 2 (break 1 ^^ print_expr e))
  in group (test ^/^ if_so ^/^ if_not)

(* Consing expression *)

and print_E_Cons (node : sharp bin_op reg) = print_bin_op node

(* Arithmetic division *)

and print_E_Div (node : slash bin_op reg) = print_bin_op node

(* Function expressions *)

and print_E_Fun (node : fun_expr reg) =
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

(* Greater or equal than *)

and print_E_Geq (node : geq bin_op reg) = print_bin_op node

(* Greater than *)

and print_E_Gt (node : gt bin_op reg) = print_bin_op node

(* Integers *)

and print_E_Int (node : (lexeme * Z.t) wrap) = print_int node

(* Lower or equal than *)

and print_E_Leq (node : leq bin_op reg) = print_bin_op node

(* List expressions *)

and print_E_List (node : expr compound reg) =
  print_compound print_expr node

(* Lower than *)

and print_E_Lt (node : lt bin_op reg) = print_bin_op node

(* Map expression *)

and print_E_Map (node : binding reg compound reg) =
  print_compount print_binding node

(* Map lookup *)

and print_E_MapLookup (node : map_lookup reg) =
  let {map; index} = node.value in
  group (print_expr map ^//^ print_brackets print_expr index)

(* Modulo *)

and print_E_Mod (node : kwd_mod bin_op reg) = print_bin_op node

(* Multiplication *)

and print_E_Mult (node : times bin_op reg) = print_bin_op node

(* Mutez as an expression *)

and print_E_Mutez (node : (lexeme * Int64.t) wrap) = print_mutez node

(* Natural numbers in expressions *)

and print_E_Nat (node :  (lexeme * Z.t) wrap) = print_nat node

(* Arithmetic negation *)

and print_E_Neg (node : minus un_op reg) = print_un_op node

and print_un_op (node : lexeme wrap un_op reg) =
  let {op; arg} = node.value in
  string (op#payload ^ " ") ^^ print_expr arg

(* The empty list as an expression *)

and print_E_Nil (node : kwd_nil) = string node#payload

(* Arithmetic difference *)

and print_E_Neq (node : neq bin_op reg) = print_bin_op node

(* Logical negation *)

and print_E_Not (node : kwd_not un_op reg) = print_un_op node

(* Logical disjunction *)

and print_E_Or (node : kwd_or bin_op reg) = print_bin_op node

(* Parenthesised expression *)

and print_E_Par (node : expr par reg) = print_par print_expr node

(* Record expression *)

and print_E_Record (node : record_expr) =
  print_compound print_field_expr node

and print_field_expr (node : (expr, expr) field reg) =
  match node.value with
    Punned {pun; attributes} ->
      let thread = print_expr pun in
      let thread = print_attributes thread attributes
      in thread
  | Complete {field_lhs; field_rhs; attributes; _} ->
      let thread = group ((print_expr field_lhs ^^ string " =")
                          ^//^ print_expr field_rhs) in
      let thread = print_attributes thread attributes
      in thread

(* XXX *)

and print_field_assign {value; _} =
  let {field_name; field_expr; _} = value in
  prefix 2 1 (print_ident field_name ^^ string " =") (print_expr field_expr)

and print_record ne_inj = group (print_ne_injection print_field_assign ne_inj)

and print_set_membership {value; _} =
  let {set; element; _} : set_membership = value in
  group (print_expr set ^/^ string "contains" ^/^ print_expr element)

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

and print_path = function
  Name v -> print_ident v
| Path p -> print_projection p

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

and print_field_path_assign {value; _} =
  let {field_path; field_expr; _} = value in
  let path = print_path field_path in
  prefix 2 1 (path ^^ string " =") (print_expr field_expr)

and print_selection = function
  FieldName v   -> string v.value
| Component cmp -> cmp.value |> snd |> Z.to_string |> string


let print_type_expr = print_type_expr
let print_pattern   = print_pattern
let print_expr      = print_expr

type cst        = CST.t
type expr       = CST.expr
type type_expr  = CST.type_expr
type pattern    = CST.pattern
