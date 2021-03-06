open Types
open Format
open Simple_utils.PP_helpers
include PP_enums

let option_inline ppf inline =
  if inline then
    fprintf ppf "[@@inline]"
  else
    fprintf ppf ""

let option_no_mutation ppf no_mutation =
  if no_mutation then
    fprintf ppf "[@@no_mutation]"
  else
    fprintf ppf ""

let option_view ppf no_mutation =
  if no_mutation then
    fprintf ppf "[@@view]"
  else
    fprintf ppf ""

let option_public ppf public =
  if not public then
    fprintf ppf "[@@private]"
  else
    fprintf ppf ""

let option_thunk ppf thunk =
  if thunk then
    fprintf ppf "[@@thunk]"
  else
    fprintf ppf ""

let option_hidden ppf hidden =
  if hidden then
    fprintf ppf "[@@hidden]"
  else
    fprintf ppf ""

let e_attributes ppf { inline ; no_mutation ; view ; public ; thunk ; hidden } =
  fprintf ppf "%a%a%a%a%a%a"
    option_inline inline
    option_no_mutation no_mutation
    option_view view
    option_public public
    option_thunk thunk
    option_hidden hidden

let label ppf (l:label) : unit =
  let Label l = l in fprintf ppf "%s" l

let expression_variable ppf (t : expression_variable) : unit = fprintf ppf "%a" ValueVar.pp t
let type_variable       ppf (t : type_variable)       : unit = fprintf ppf "%a" TypeVar.pp t
let module_variable     ppf (t : module_variable)     : unit = fprintf ppf "%a" ModuleVar.pp t
let kind_               ppf (_ : kind)                : unit = fprintf ppf "*"

and access f ppf a =
  match a with
    | Access_tuple i  -> fprintf ppf "%a" Z.pp_print i
    | Access_record s -> fprintf ppf "%s" s
    | Access_map e    -> fprintf ppf "[%a]" f e

let record_sep value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst
let variant_sep_d x = record_sep x (tag " ,@ ")

let tuple_sep value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_, {associated_type;_}) = fprintf ppf "%a" value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let layout ppf layout = match layout with
  | L_tree -> fprintf ppf "tree"
  | L_comb -> fprintf ppf "comb"

let option_layout ppf l = match l with
  | Some l -> fprintf ppf "[layout:%a]" layout l
  | None   -> fprintf ppf ""

let layout_option = option layout

let record_sep_expr value sep ppf (m : 'a label_map) =
  let lst = LMap.to_kv_list m in
  let lst = List.dedup_and_sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) lst in
  let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let tuple_sep_expr value sep ppf m =
  assert (Helpers.is_tuple_lmap m);
  let lst = Helpers.tuple_of_record m in
  let new_pp ppf (_,v) = fprintf ppf "%a" value v in
  fprintf ppf "%a" (list_sep new_pp sep) lst

(* Prints records which only contain the consecutive fields
  0..(cardinal-1) as tuples *)
let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep value (tag sep_record)) m

let tuple_or_record_sep_expr value format_record sep_record format_tuple sep_tuple ppf m =
  if Helpers.is_tuple_lmap m then
    fprintf ppf format_tuple (tuple_sep_expr value (tag sep_tuple)) m
  else
    fprintf ppf format_record (record_sep_expr value (tag sep_record)) m

let tuple_or_record_sep_expr value = tuple_or_record_sep_expr value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " ,@ "
let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " *@ "

let attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat
  in fprintf ppf "%s" attr

let module_access f ppf = fun {module_path;element} ->
  fprintf ppf "%a.%a" (list_sep (module_variable) (const ".")) module_path f element

(* Types *)
let for_all type_expression ppf ({ty_binder ; kind ; type_}) : unit =
  fprintf ppf "??? (%a : %a) . %a" type_variable ty_binder kind_ kind type_expression type_

let abstraction type_expression ppf ({ty_binder ; kind = _ ; type_}) : unit =
  fprintf ppf "fun %a . %a" type_variable ty_binder type_expression type_

let type_app type_expression ppf ({type_operator ; arguments}: 'a type_app) : unit =
  fprintf ppf "%a%a" type_variable type_operator (list_sep_d_par type_expression) arguments

let sum type_expression ppf = fun sum ->
  fprintf ppf "@[<hv 4>sum[%a]@]" (variant_sep_d type_expression) sum

let type_record type_expression ppf = fun record ->
  fprintf ppf "%a" (tuple_or_record_sep_type type_expression) record

let type_tuple type_expression ppf = fun tuple ->
  fprintf ppf "(%a)" (list_sep type_expression (tag " , ")) tuple

let arrow type_expression ppf = fun {type1;type2} ->
  fprintf ppf "%a -> %a" type_expression type1 type_expression type2

let wildcard ppf = fun () ->
  fprintf ppf "_"

(* Expressions *)

let option_const_or_var ppf is_var =
  match is_var with
  | None -> fprintf ppf ""
  | Some `Var -> fprintf ppf "[@var]"
  | Some `Const -> fprintf ppf ""

let binder type_expression ppf {var;ascr;attributes={const_or_var}} =
  match ascr with
  | None ->
      fprintf ppf "%a%a" expression_variable var option_const_or_var const_or_var
  | Some ty ->
      fprintf ppf "%a%a : %a" expression_variable var option_const_or_var const_or_var type_expression ty

let application expression ppf = fun {lamb;args} ->
  fprintf ppf "@[<hv>(%a)@@(%a)@]" expression lamb expression args

let constructor expression ppf = fun {constructor;element} ->
  fprintf ppf "@[%a(%a)@]" label constructor expression element

let constant expression ppf = fun {cons_name;arguments} ->
  fprintf ppf "@[%a@[<hv 1>(%a)@]@]" constant' cons_name (list_sep_d expression) arguments

let record expression ppf = fun r ->
  fprintf ppf "%a" (tuple_or_record_sep_expr expression) r

let record_accessor expression ppf = fun ({record;path}: _ record_accessor) ->
  fprintf ppf "@[%a.%a@]" expression record label path

let record_update expression ppf = fun {record; path; update} ->
  fprintf ppf "@[{ %a@;<1 2>with@;<1 2>{ %a = %a } }@]" expression record label path expression update

let tuple expression ppf = fun t ->
  fprintf ppf "(@[<h>%a@])" (list_sep expression (tag " , ")) t

let accessor expression ppf = fun ({record;path}: _ accessor) ->
  fprintf ppf "%a.%a" expression record (list_sep (access expression) (const ".")) path

let update expression ppf = fun ({record; path; update}:_ update) ->
  fprintf ppf "{ %a with %a = %a }" expression record (list_sep (access expression) (const ".")) path expression update

let raw_code expression ppf = fun {language; code} ->
  fprintf ppf "[%%%s %a]" language expression code

let option_type_expression type_expression ppf = function
  None -> fprintf ppf ""
| Some te -> fprintf ppf " : %a" type_expression te

let lambda expression type_expression ppf = fun {binder=b; output_type; result} ->
  fprintf ppf "lambda (%a)%a return %a"
    (binder type_expression) b
    (option_type_expression type_expression) output_type
    expression result

let type_abs expression ppf = fun {type_binder;result} ->
  fprintf ppf "?? %a -> @;@[%a@]"
    type_variable type_binder
    expression result

let _option_map ppf (k,v_opt) =
  match v_opt with
  | None -> fprintf ppf "%a" expression_variable k
  | Some v -> fprintf ppf "%a -> %a" expression_variable k expression_variable v


and single_record_patch expression ppf ((p, expr) : label * 'expr) =
  fprintf ppf "%a <- %a" label p expression expr
let recursive expression type_expression ppf = fun { fun_name;fun_type; lambda=l} ->
  fprintf ppf "rec (%a:%a => %a )"
    expression_variable fun_name
    type_expression fun_type
    (lambda expression type_expression) l

let let_in expression type_expression ppf = fun {let_binder; rhs; let_result; attributes=attr} ->
  fprintf ppf "@[<v>let %a = %a%a in@,%a@]"
    (binder type_expression) let_binder
    expression rhs
    attributes attr
    expression let_result

let type_in expression type_expression ppf = fun {type_binder; rhs; let_result} ->
  fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
    type_variable type_binder
    type_expression rhs
    expression let_result

let ascription expression type_expression ppf = fun {anno_expr; type_annotation} ->
  fprintf ppf "%a : %a"
    expression anno_expr
    type_expression type_annotation

let cond expression ppf = fun {condition; then_clause; else_clause} ->
  fprintf ppf "if %a then %a else %a"
    expression condition
    expression then_clause
    expression else_clause

let sequence expression ppf = fun {expr1;expr2} ->
  fprintf ppf "{@[<v 2>@,%a;@,%a@]@,}"
    expression expr1
    expression expr2

let skip ppf = fun () ->
  fprintf ppf "skip"

let assoc_expression expression ppf : 'exp * 'exp -> unit =
 fun (a, b) -> fprintf ppf "%a -> %a" expression a expression b

let map expression ppf = fun m ->
  fprintf ppf "map[%a]" (list_sep_d (assoc_expression expression)) m

let big_map expression ppf = fun m ->
  fprintf ppf "big_map[%a]" (list_sep_d (assoc_expression expression)) m

let lst expression ppf = fun lst ->
  fprintf ppf "list[%a]" (list_sep_d expression) lst

let set expression ppf = fun set ->
  fprintf ppf "set[%a]" (list_sep_d expression) set

let assign expression type_expression ppf = fun {binder=b; access_path; expression=e} ->
  fprintf ppf "%a%a := %a"
    (binder type_expression) b
    (list_sep_prep (access expression) (const ".")) access_path
    expression e

let for_ expression ppf = fun {binder; start; final; incr; f_body} ->
  fprintf ppf "for %a from %a to %a by %a do %a"
    expression_variable binder
    expression start
    expression final
    expression incr
    expression f_body

let option_map ppf (k,v_opt) =
  match v_opt with
  | None -> fprintf ppf "%a" expression_variable k
  | Some v -> fprintf ppf "%a -> %a" expression_variable k expression_variable v

let for_each expression ppf = fun {fe_binder; collection; fe_body; _} ->
  fprintf ppf "for each %a in %a do %a"
    option_map fe_binder
    expression collection
    expression fe_body

let while_ expression ppf = fun {cond; body} ->
  fprintf ppf "while %a do %a"
    expression cond
    expression body

(* matches *)

let rec list_pattern type_expression ppf = fun pl ->
  let mpp = match_pattern type_expression in
  match pl with
  | Cons (pl,pr) -> fprintf ppf "%a :: %a" mpp pl mpp pr
  | List pl -> fprintf ppf "[ %a ]" (list_sep mpp (tag " ; ")) pl

and match_pattern type_expression ppf = fun p ->
  match p.wrap_content with
  | P_unit -> fprintf ppf "()"
  | P_var b -> fprintf ppf "%a" (binder type_expression) b
  | P_list l -> list_pattern type_expression ppf l
  | P_variant (l , p) -> fprintf ppf "%a %a" label l (match_pattern type_expression) p
  | P_tuple pl ->
    fprintf ppf "(%a)" (list_sep (match_pattern type_expression) (tag ",")) pl
  | P_record (ll , pl) ->
    let x = List.zip_exn ll pl in
    let aux ppf (l,p) =
      fprintf ppf "%a = %a" label l (match_pattern type_expression) p
    in
    fprintf ppf "{%a}" (list_sep aux (tag " ; ")) x

let match_case expression type_expression ppf = fun {pattern ; body} ->
  fprintf ppf "@[| %a -> %a@]" (match_pattern type_expression) pattern expression body

let match_exp expression type_expression ppf = fun {matchee ; cases} ->
  fprintf ppf "@[<v 2> match %a with@,%a@]" expression matchee (list_sep (match_case expression type_expression) (tag "@ ")) cases
let rec mod_in expression type_expression a_e a_t a_m ppf = fun {module_binder; rhs; let_result;} ->
  fprintf ppf "@[module %a =@;<1 2>%a in@ %a@]"
    module_variable module_binder
    (module_expr expression type_expression a_e a_t a_m) rhs
    expression let_result
(* Declaration *)
and declaration_type type_expression a_t ppf = fun {type_binder;type_expr; type_attr} ->
  fprintf ppf "@[<2>type %a =@ %a%a@]"
    type_variable type_binder
    type_expression type_expr
    a_t type_attr

and declaration_constant ?(print_type = true) expression type_expression a_e ppf = fun {binder=binder'; attr ; expr} ->
  let cond ppf b =
    if print_type then
      fprintf ppf "%a" (binder type_expression) b
    else
      fprintf ppf "%a" expression_variable b.var
  in
  fprintf ppf "@[<2>const %a =@ %a%a@]"
    cond binder'
    expression expr
    a_e attr

and declaration_module ?(print_type = true) expression type_expression a_e a_t a_m ppf = fun {module_binder;module_;module_attr} ->
  fprintf ppf "@[<2>module %a =@ %a%a@]"
    module_variable module_binder
    (module_expr ~print_type expression type_expression a_e a_t a_m) module_
    a_m module_attr

and declaration_content ?(print_type = true) expression type_expression a_e a_t a_m ppf = function
    Declaration_type    ty -> declaration_type type_expression a_t ppf ty
  | Declaration_constant c -> declaration_constant ~print_type expression type_expression a_e ppf c
  | Declaration_module   m -> declaration_module ~print_type expression type_expression a_e a_t a_m ppf m
and declaration ?(print_type = true) expression type_expression a_e a_t a_m ppf x =
  declaration_content ~print_type expression type_expression a_e a_t a_m ppf (Location.unwrap x)
and declarations ?(print_type = true) expression type_expression a_e a_t a_m ppf x =
  fprintf ppf "@[<v>%a@]"
    (list_sep (declaration ~print_type expression type_expression a_e a_t a_m) (tag "@,"))
    x
and module_expr_content ?(print_type = true) expression type_expression a_e a_t a_m ppf = function
  | M_struct p ->
    fprintf ppf "@[<v>struct@,%a@,end@]"
      (declarations ~print_type expression type_expression a_e a_t a_m) p
  | M_variable x -> module_variable ppf x
  | M_module_path path ->
    (ne_list_sep (module_variable) (tag ".")) ppf path

and module_expr ?(print_type = true) expression type_expression a_e a_t a_m ppf x =
  module_expr_content ~print_type expression type_expression a_e a_t a_m ppf (Location.unwrap x)
