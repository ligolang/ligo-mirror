open Types
open Format
open Simple_utils.PP_helpers

include Stage_common.PP

let sum_set_t value sep ppf m =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, {associated_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value associated_type in
  fprintf ppf "%a" (list_sep new_pp sep) lst

let sum_set_t x = sum_set_t x (tag " ,@ ")

let attributes_2 (attr: string list) : string =
  List.map ~f:(fun s -> "[@@" ^ s ^ "]") attr |> String.concat

let attributes_1 (attr: string list) : string =
  List.map ~f:(fun s -> "[@" ^ s ^ "]") attr |> String.concat


let record_sep_t value sep ppf (m : (label * type_expression row_element) list) =
  let lst = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) m in
  let new_pp ppf (k, {associated_type; attributes; _}) =
    let attr = attributes_2 attributes in
    fprintf ppf "@[<h>%a -> %a %s@]" label k value associated_type attr
  in fprintf ppf "%a" (list_sep new_pp sep) lst

let rec type_content : formatter -> type_expression -> unit =
  fun ppf te ->
  match te.type_content with
  | T_sum m -> (
    let s ppf = fprintf ppf "@[<hv 4>sum[%a]@]" (sum_set_t type_expression) in
    match m.attributes with [] -> fprintf ppf "%a" s m.fields
    |_ ->
      let attr = attributes_1 m.attributes in
      fprintf ppf "(%a %s)" s m.fields attr
  )
  | T_record m -> (
    let r = record_sep_t type_expression (const ";") in
    match m.attributes with
    | [] -> fprintf ppf "{%a}" r m.fields
    | _ ->
      let attr : string = attributes_1 m.attributes in
      fprintf ppf "({%a} %s)" r m.fields attr
  )
  | T_variable        tv -> type_variable ppf tv
  | T_tuple            t -> type_tuple    type_expression ppf t
  | T_arrow            a -> arrow         type_expression ppf a
  | T_annoted  (ty, str) -> fprintf ppf "(%a%%%s)" type_expression ty str
  | T_app            app -> type_app      type_expression ppf app
  | T_module_accessor ma -> module_access type_variable ppf ma
  | T_singleton       x  -> literal       ppf             x
  | T_abstraction     x  -> abstraction   type_expression ppf x
  | T_for_all         x  -> for_all       type_expression ppf x

and type_expression ppf (te : type_expression) : unit =
  fprintf ppf "%a" type_content te

let rec expression ppf (e : expression) =
  fprintf ppf "%a" expression_content e.expression_content
and expression_content ppf (ec : expression_content) =
  match ec with
  | E_literal     l -> literal                ppf l
  | E_variable    n -> expression_variable    ppf n
  | E_application a -> application expression ppf a
  | E_constructor c -> constructor expression ppf c
  | E_constant c ->
      fprintf ppf "%a(%a)"
        constant' (const_name c.cons_name)
        (list_sep expression (tag " ,")) c.arguments
  | E_record      r -> record ppf r
  | E_tuple       t -> tuple       expression ppf t
  | E_accessor    a -> accessor    expression ppf a
  | E_update      u -> update      expression ppf u
  | E_lambda      l -> lambda      expression type_expression ppf l
  | E_type_abstraction e -> type_abs expression ppf e
  | E_matching    m -> match_exp expression type_expression ppf m
  | E_recursive  r -> recursive expression type_expression ppf r
  | E_let_in    li -> let_in  expression type_expression ppf li
  | E_type_in   ti -> type_in expression type_expression ppf ti
  | E_mod_in    mi -> mod_in  expression type_expression attributes attributes attributes ppf mi
  | E_raw_code   r -> raw_code   expression ppf r
  | E_ascription a -> ascription expression type_expression ppf a
  | E_module_accessor ma -> module_access expression_variable ppf ma
  | E_cond       c -> cond       expression ppf c
  | E_sequence   s -> sequence   expression ppf s
  | E_skip         -> skip                  ppf ()
  | E_map        m -> map        expression ppf m
  | E_big_map    m -> big_map    expression ppf m
  | E_list       l -> lst        expression ppf l
  | E_set        s -> set        expression ppf s
  | E_assign     a -> assign     expression type_expression ppf a
  | E_for        f -> for_       expression ppf f
  | E_for_each   f -> for_each   expression ppf f
  | E_while      w -> while_     expression ppf w

and record ppf kvl =
  fprintf ppf "@[<v>{ %a }@]" (list_sep (fun ppf (l,e)-> Format.fprintf ppf "%a : %a" label l expression e) (tag " ;")) kvl

and attributes ppf attributes =
  let attr =
    List.map ~f:(fun attr -> "[@@" ^ attr ^ "]") attributes |> String.concat
  in fprintf ppf "%s" attr

let declaration ppf (d : declaration) = declaration expression type_expression attributes attributes attributes ppf d

let module_ ppf (p : module_) = declarations expression type_expression attributes attributes attributes ppf p
