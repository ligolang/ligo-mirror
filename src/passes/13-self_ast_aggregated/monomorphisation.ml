let fold_map_expression = Helpers.fold_map_expression

open Ast_aggregated

type longident = { variable : expression_variable }

let poly_counter = ref 0
let poly_name v = poly_counter := ! poly_counter + 1 ;
                  Var.of_name ("poly_" ^ (Var.to_name v) ^ "_" ^ string_of_int (! poly_counter))

let equal_longident ({ variable } : longident) ({ variable = variable' } : longident) =
  equal_expression_variable variable variable'

let compare_longident ({ variable } : longident) ({ variable = variable' } : longident) =
  compare_expression_variable variable variable'

let pp_longident ppf ({ variable } : longident) =
  Format.fprintf ppf "%a" PP.expression_variable variable

let lident_of_variable variable : longident = { variable }

(* This is a polymorphic instance of the polymorphic function (or value) lid *)
type instance = { vid : longident ; type_instances : type_expression list ; type_ : type_expression }

let pp_instance ppf { vid ; type_instances ; type_ } =
  Format.fprintf ppf "{ vid = %a ; type_ = %a ; type_instances = [ %a ] }"
    pp_longident vid PP.type_expression type_ (PP_helpers.list_sep_d PP.type_expression) type_instances

module LIMap = Map.Make(struct type t = longident let compare x y = compare_longident x y end)

type data = { env : unit ; instances : (instance list) LIMap.t }
let empty_data : data = { env = () ; instances = LIMap.empty }

let pp_data ppf { instances } =
  let f (lid, instances_of_lid) =
    Format.fprintf ppf "{ lid = %a ~> %a }"
    pp_longident lid (PP_helpers.list_sep_d pp_instance) instances_of_lid
  in
  List.iter (LIMap.to_kv_list instances) ~f

let instances_lookup (lid : longident) (data : data) =
  Option.value ~default:[] @@ LIMap.find_opt lid data.instances

let instances_lookup_and_remove (lid : longident) (data : data) =
  Option.value ~default:[] @@ LIMap.find_opt lid data.instances,
  { data with instances = LIMap.add lid [] data.instances }

let instance_lookup_opt (lid : longident) (type_instances' : type_expression list) (type_' : type_expression) (data : data) =
  let rec aux = function
    | [] -> None
    | { vid ; type_instances ; type_ } :: _
         when Helpers.type_expression_eq (type_, type_') &&
              List.equal (fun t1 t2 -> Helpers.type_expression_eq (t1, t2)) type_instances type_instances' ->
       Some (vid, type_instances)
    | _ :: tl -> aux tl in
  aux @@ Option.value ~default:[] (LIMap.find_opt lid data.instances)

let instance_add (lid : longident) (instance : instance) (data : data) =
  let lid_instances = instance :: (Option.value ~default:[] @@ LIMap.find_opt lid data.instances) in
  { data with instances = LIMap.add lid lid_instances data.instances }

let instances_add (lid : longident) (instances : instance list) (data : data) =
  let lid_instances = instances @ (Option.value ~default:[] @@ LIMap.find_opt lid data.instances) in
  { data with instances = LIMap.add lid lid_instances data.instances }

(* This is not a proper substitution, it might capture variables: it should be used only with v' a fresh variable *)
let rec subst_var_expr v v' (e : expression) =
  let (), e = fold_map_expression (fun () e ->
                  let (=) = equal_expression_variable in
                  let return expression_content = (true, (), { e with expression_content }) in
                  let return_f expression_content = (false, (), { e with expression_content }) in
                  match e.expression_content with
                  | E_variable w when (w = v) -> return @@ E_variable v'
                  | E_lambda { binder ; result } when (binder = v) -> return_f @@ E_lambda { binder ; result }
                  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } when (fun_name = v) || (binder = v) ->
                     return_f @@ E_recursive { fun_name ; fun_type ; lambda = { binder ; result } }
                  | E_let_in { let_binder ; rhs ; let_result ; attr } when (let_binder = v) ->
                     let rhs = subst_var_expr v v' rhs in
                     return_f @@ E_let_in { let_binder ; rhs ; let_result ; attr }
                  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
                     let f = function
                         ({ constructor ; pattern ; body } : matching_content_case) when not (pattern = v) ->
                          let body = subst_var_expr v v' body in
                          { constructor ; pattern ; body }
                       | cc -> cc in
                     let matchee = subst_var_expr v v' matchee in
                     let cases = List.map cases ~f in
                     return_f @@ E_matching { matchee ; cases = Match_variant { cases ; tv } }
                  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } }
                       when List.mem (List.map (LMap.to_list fields) ~f:fst) v ~equal:(=)  ->
                     let matchee = subst_var_expr v v' matchee in
                     return_f @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv } }
                  | _ -> return e.expression_content
                  ) () e in
  e

let apply_table_expr table (e : expression) =
  let apply_table_type u = List.fold_right table ~f:(fun (v, t) u -> Helpers.subst_type v t u) ~init:u in
  let (), e = fold_map_expression (fun () e ->
                  let e = { e with type_expression = apply_table_type e.type_expression } in
                  let return expression_content = (true, (), { e with expression_content }) in
                  match e.expression_content with
                  | E_type_inst { forall ; type_ } ->
                     return @@ E_type_inst { forall ; type_ = apply_table_type type_ }
                  | E_recursive { fun_name ; fun_type ; lambda } ->
                     let fun_type = apply_table_type fun_type in
                     return @@ E_recursive { fun_name ; fun_type ; lambda }
                  | E_matching { matchee ; cases = Match_variant { cases ; tv } } ->
                     return @@ E_matching { matchee ; cases = Match_variant { cases ; tv = apply_table_type tv } }
                  | E_matching { matchee ; cases = Match_record { fields ; body ; tv } } ->
                     let fields = LMap.map (fun (casev, caset) -> (casev, apply_table_type caset)) fields in
                     return @@ E_matching { matchee ; cases = Match_record { fields ; body ; tv = apply_table_type tv } }
                  | _ -> return e.expression_content) () e in
  e

let longident_to_expression (lid : longident) type_ =
  make_e (E_variable lid.variable) type_

let rec mono_polymorphic_expression : _ -> data -> expression -> data * expression = fun path data expr ->
  let self = mono_polymorphic_expression path in
  let return ec = { expr with expression_content = ec } in
  match expr.expression_content with
  | E_variable _ | E_literal _ | E_raw_code _ -> data, expr
  | E_constant { cons_name ; arguments } ->
     let data, arguments = List.fold_right arguments ~init:(data, [])
                             ~f:(fun arg (data, args) -> let data, arg = self data arg in data, arg :: args) in
     data, return (E_constant { cons_name ; arguments })
  | E_application { lamb ; args } ->
     let data, lamb = self data lamb in
     let data, args = self data args in
     data, return (E_application { lamb; args })
  | E_lambda { binder ; result } ->
     let binder_instances, data = instances_lookup_and_remove { variable = binder } data in
     let data, result = self data result in
     let _, data = instances_lookup_and_remove { variable = binder } data in
     let data = instances_add  { variable = binder } binder_instances data in
     data, return (E_lambda { binder ; result })
  | E_recursive { fun_name ; fun_type ; lambda = { binder ; result } } ->
     let data, result = self data result in
     let _, data = instances_lookup_and_remove { variable = binder } data in
     let _, data = instances_lookup_and_remove { variable = fun_name } data in
     data, return (E_recursive { fun_name ; fun_type ; lambda = { binder ; result } })
  | E_let_in { let_binder ; rhs ; let_result ; attr } -> (
    match rhs.type_expression.type_content with
    | T_for_all _ ->
       let type_vars, type_ = Helpers.destruct_for_alls rhs.type_expression in
       let build_let (lid : longident) { vid ; type_instances ; type_ = typed } (let_result, data) =
         let let_binder = vid.variable in
         let table = List.zip_exn type_vars type_instances in
         let rhs = { rhs with type_expression = type_ } in
         let rhs, data = match rhs.expression_content with
           | E_recursive { fun_name = _ ; fun_type = _ ; lambda = { binder ; result } } ->
              let lambda = { binder ; result = subst_var_expr lid.variable vid.variable result } in
              let data = instance_add lid { vid ; type_instances ; type_ = typed } data in
              { rhs with expression_content = E_recursive { fun_name = vid.variable ; fun_type = type_ ; lambda } }, data
           | _ -> rhs, data in
         let rhs = apply_table_expr table rhs in
         let data, rhs = self data rhs in
         (e_a_let_in let_binder rhs let_result attr, data) in
       let data, let_result = self data let_result in
       let instances, data = instances_lookup_and_remove (lident_of_variable let_binder) data in
       let expr, data = List.fold_right instances ~f:(build_let @@ lident_of_variable let_binder) ~init:(let_result, data) in
       data, expr
    | _ ->
       let binder_instances, data = instances_lookup_and_remove (lident_of_variable let_binder) data in
       let data, let_result = self data let_result in
       let _, data = instances_lookup_and_remove (lident_of_variable let_binder) data in
       let data = instances_add (lident_of_variable let_binder) binder_instances data in
       let data, rhs = self data rhs in
       data, return (E_let_in { let_binder ; rhs ; let_result ; attr })
  )
  | E_type_in { type_binder ; rhs ; let_result } ->
     let data, let_result = self data let_result in
     data, return (E_type_in { type_binder ; rhs ; let_result })
  | E_constructor { constructor ; element } ->
     let data, element  = self data element in
     data, return (E_constructor { constructor ; element })
  | E_matching { matchee ; cases } ->
     let data, cases = mono_polymorphic_cases path data cases in
     let data, matchee = self data matchee in
     data, return (E_matching { matchee ; cases })
  | E_record lmap ->
     let data, lmap = LMap.fold (fun l expr (data, r) ->
                          let data, v = self data expr in
                          data, LMap.add l v r) lmap (data, LMap.empty) in
     data, return (E_record lmap)
  | E_record_accessor { record ; path } ->
     let data, record = self data record in
     data, return (E_record_accessor { record ; path })
  | E_record_update { record ; path ; update } ->
     let data, update = self data update in
     let data, record = self data record in
     data, return (E_record_update { record ; path ; update })
  | E_type_inst _ ->
     let rec aux type_insts path (e : expression) = match e.expression_content with
       | E_type_inst {forall; type_} ->
          aux (type_ :: type_insts) path forall
       | E_variable variable -> (
         (List.rev type_insts, { variable }))
       | _ -> failwith "Cannot resolve non-variables with instantiations" in
     let type_instances, lid = aux [] [] expr in
     let type_ = expr.type_expression in
     let vid, data = match instance_lookup_opt lid type_instances type_ data with
       | Some (vid, _) -> vid, data
       | None ->
          let vid = { variable = (Location.wrap (poly_name (Location.unwrap lid.variable))) } in
          vid, instance_add lid { vid ; type_instances ; type_ } data in
     data, longident_to_expression vid type_

and mono_polymorphic_cases : _ -> data -> matching_expr -> data * matching_expr = fun path data m ->
  match m with
  | Match_variant { tv ; cases } ->
     let aux { constructor ; pattern ; body } (data, r) =
       let binder_instances, data = instances_lookup_and_remove (lident_of_variable pattern) data in
       let data, body = mono_polymorphic_expression path data body in
       let _, data = instances_lookup_and_remove (lident_of_variable pattern) data in
       let data = instances_add (lident_of_variable pattern) binder_instances data in
       data, { constructor ; pattern ; body} :: r in
     let data, cases = List.fold_right cases ~f:aux ~init:(data, []) in
     data, Match_variant { tv ; cases }
  | Match_record { tv ; body ; fields } ->
     let binders = List.map ~f:fst @@ LMap.to_list fields in
     let data, binders_instances = List.fold_right binders ~init:(data, []) ~f:(fun binder (data, binders_instances) ->
                                      let binder_instances, data = instances_lookup_and_remove (lident_of_variable binder) data in
                                      data, (binder, binder_instances) :: binders_instances) in
     let data, body = mono_polymorphic_expression path data body in
     let data = List.fold_right binders ~init:data ~f:(fun binder data ->
                    let _, data = instances_lookup_and_remove (lident_of_variable binder) data in data) in
     let data = List.fold_right binders_instances ~init:data ~f:(fun (binder, binder_instances) data ->
                    instances_add (lident_of_variable binder) binder_instances data) in
     data, Match_record { tv ; body ; fields }

let mono_polymorphic_expr e =
  let _, m = mono_polymorphic_expression [] empty_data e in
  m
