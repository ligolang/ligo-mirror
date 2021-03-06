module I = Ast_sugar
module O = Ast_core

module Location = Simple_utils.Location
module Pair     = Simple_utils.Pair
open Stage_common.Maps

let decompile_exp_attributes : O.known_attributes -> I.attributes = fun { inline ; no_mutation ; view ; public ; thunk ; hidden } ->
  let aux : string list -> (unit -> string option) -> O.attributes = fun acc is_fun ->
    match is_fun () with
    | Some v -> v::acc
    | None -> acc
  in
  List.fold ~init:[] ~f:aux
    [
      (fun () -> if inline then Some "inline" else None) ;
      (fun () -> if no_mutation then Some "no_mutation" else None) ;
      (fun () -> if view then Some "view" else None) ;
      (fun () -> if public then None else Some "private") ;
      (fun () -> if thunk then Some "thunk" else None) ;
      (fun () -> if hidden then Some "hidden" else None) ;
    ]

let decompile_type_attributes : O.type_attribute -> I.attributes = fun { public ; hidden } ->
  let aux : string list -> (unit -> string option) -> O.attributes = fun acc is_fun ->
    match is_fun () with
    | Some v -> v::acc
    | None -> acc
  in
  List.fold ~init:[] ~f:aux
    [
      (fun () -> if public then None else Some "private") ;
      (fun () -> if hidden then Some "hidden" else None) ;
    ]
let decompile_module_attributes = decompile_type_attributes

let rec decompile_type_expression : O.type_expression -> I.type_expression =
  fun te ->
  let self = decompile_type_expression in
  let return te = I.make_t te in
  match te.sugar with
    Some te -> te
  | None ->
    match te.type_content with
      | O.T_variable type_variable -> return @@ T_variable type_variable
      | O.T_app tc ->
        let tc = type_app self tc in
        return @@ T_app tc
      | O.T_sum {fields;layout} ->
        let fields =
          O.LMap.map (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ I.row_element = {associated_type;attributes;decl_pos} in
            v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" O.PP.layout l))] | None -> [] in
        return @@ I.T_sum {fields ; attributes}
      | O.T_record {fields;layout} ->
        let fields =
          O.LMap.map (fun v ->
            let {associated_type;michelson_annotation;decl_pos} : O.row_element = v in
            let associated_type = self associated_type in
            let attributes = match michelson_annotation with | Some a -> [a] | None -> [] in
            let v' : _ I.row_element = {associated_type ; attributes ; decl_pos} in
            v'
          ) fields
        in
        let attributes = match layout with Some l -> [("layout:"^(Format.asprintf "%a" O.PP.layout l))] | None -> [] in
        return @@ I.T_record { fields ; attributes }
      | O.T_arrow arr ->
        let arr = arrow self arr in
        return @@ T_arrow arr
      | O.T_module_accessor ma -> return @@ T_module_accessor ma
      | O.T_singleton x -> return @@ I.T_singleton x
      | O.T_abstraction x ->
        let type_ = self x.type_ in
        return @@ I.T_abstraction { x with type_ }
      | O.T_for_all x ->
        let type_ = self x.type_ in
        return @@ I.T_for_all { x with type_ }

let rec decompile_expression : O.expression -> I.expression =
  fun e ->
  let self = decompile_expression in
  let self_type = decompile_type_expression in
  let return expr = I.make_e ~loc:e.location expr in
  match e.sugar with
    Some e -> e
  | None ->
    match e.expression_content with
      O.E_literal lit -> return @@ I.E_literal (lit)
    | O.E_constant {cons_name;arguments} ->
      let arguments = List.map ~f:self arguments in
      return @@ I.E_constant {cons_name = cons_name;arguments}
    | O.E_variable name -> return @@ I.E_variable name
    | O.E_application app ->
      let app = application self app in
      return @@ I.E_application app
    | O.E_lambda lamb ->
      let lamb = lambda self self_type lamb in
      return @@ I.E_lambda lamb
    | O.E_type_abstraction ta ->
      let ta = type_abs self ta in
      return @@ I.E_type_abstraction ta
    | O.E_recursive recs ->
      let recs = recursive self self_type recs in
      return @@ I.E_recursive recs
    | O.E_let_in {let_binder = {var; ascr;attributes=_};attr={inline=false;no_mutation=_;view=_;public=_;thunk=_;hidden=_};rhs=expr1;let_result=expr2}
      when O.ValueVar.is_name var "()"
           && Stdlib.(=) ascr (Some (O.t_unit ())) ->
      let expr1 = self expr1 in
      let expr2 = self expr2 in
      return @@ I.E_sequence {expr1;expr2}
    | O.E_let_in {let_binder;attr;rhs;let_result} ->
      let let_binder = binder self_type let_binder in
      let rhs = self rhs in
      let let_result = self let_result in
      let attributes = if attr.inline then ["inline"] else [] in
      return @@ I.E_let_in {let_binder;mut=false;attributes;rhs;let_result}
    | O.E_type_in {type_binder; rhs; let_result} ->
      let rhs = self_type rhs in
      let let_result = self let_result in
      return @@ I.E_type_in {type_binder; rhs; let_result}
    | O.E_mod_in mi ->
      let f = Stage_common.Maps.mod_in
        decompile_expression
        decompile_type_expression
        decompile_exp_attributes
        decompile_type_attributes
        decompile_module_attributes
      in
      return @@ I.E_mod_in (f mi)
    | O.E_raw_code rc ->
      let rc = raw_code self rc in
      return @@ I.E_raw_code rc
    | O.E_constructor const ->
      let const = constructor self const in
      return @@ I.E_constructor const
    | O.E_matching {matchee; cases} ->
      let matchee = self matchee in
      let aux :
        (O.expression, O.type_expression) O.match_case -> (I.expression, I.type_expression) I.match_case =
          fun {pattern ; body} ->
            let body = self body in
            let pattern = Stage_common.Helpers.map_pattern_t (binder self_type) pattern in
            I.{pattern ; body}
      in
      let cases = List.map ~f:aux cases in
      return @@ I.E_matching {matchee ; cases}
    | O.E_record record ->
      let record = O.LMap.to_kv_list_rev record in
      let record =
        List.map ~f:(fun (O.Label k,v) ->
          let v = self v in
          (I.Label k,v)
        ) record
      in
      return @@ I.E_record (I.LMap.of_list record)
    | O.E_record_accessor {record;path} ->
      let record = self record in
      let Label path  = path in
      return @@ I.E_accessor {record;path=[I.Access_record path]}
    | O.E_record_update {record;path;update} ->
      let record = self record in
      let update = self update in
      let Label path  = path in
      return @@ I.E_update {record;path=[I.Access_record path];update}
    | O.E_ascription {anno_expr; type_annotation} ->
      let anno_expr = self anno_expr in
      let type_annotation = decompile_type_expression type_annotation in
      return @@ I.E_ascription {anno_expr; type_annotation}
    | O.E_module_accessor ma -> return @@ E_module_accessor ma
    | O.E_assign a ->
      let a = assign self self_type a in
      return @@ I.E_assign a

and decompile_lambda : _ O.lambda -> _ I.lambda =
  fun {binder=b;output_type;result}->
    let binder = binder decompile_type_expression b in
    let output_type = Option.map ~f:decompile_type_expression output_type in
    let result = decompile_expression result in
    I.{binder;output_type;result}

and decompile_module : O.module_ -> I.module_ = fun m ->
  (* List.map ~f:(Location.map decompile_declaration) m *)
  let pass = Stage_common.Maps.declarations
    decompile_expression
    decompile_type_expression
    decompile_exp_attributes
    decompile_type_attributes
    decompile_module_attributes
  in
  pass m
