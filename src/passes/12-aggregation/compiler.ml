module I = Ast_typed
module O = Ast_aggregated

(* README
  aggregates declarations : Declaration_constant -> E_let_in
  morph modules : Declaration_module -> E_let_in (E_record ..)
*)

module Mod_env = struct
  (* env used to remember module types as they are morphed into modules *)
  module ModMap = Map.Make(struct type t = string let compare = String.compare end)
  type t = (O.row_element O.label_map) ModMap.t
  let get_ty : t -> string -> O.type_expression = fun env name ->
    match ModMap.find_opt name env with
    | Some content -> O.t_record ~layout:I.default_layout content
    | None -> failwith "no module 'name'"
  let add : t -> string * O.type_expression -> t =
    fun env (var, t) ->
      let ty = O.get_t_record_exn t in
      ModMap.add var ty.content env
  let empty = ModMap.empty
end

let rec compile : I.module_fully_typed -> O.expression =
  fun (Module_Fully_Typed module_) -> compile_declaration Mod_env.empty module_

and compile_declaration : Mod_env.t -> I.declaration_loc list -> O.expression =
  fun mod_env lst ->
    match lst with
    | hd::tl -> (
      let aggregate ?(new_env = mod_env) (binder, expr, attr) = O.e_a_let_in binder expr (compile_declaration new_env tl) attr in
      let skip (*env*) = compile_declaration mod_env tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip
      | I.Declaration_constant { name = _ ; binder ; expr ; attr } -> (
        let expr = compile_expression mod_env expr in
        aggregate (binder, expr, attr)
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr } -> (
        let (content, ty) = module_to_record mod_env module_ in
        let mod_as_record = O.make_e (O.E_record content) ty in
        let binder = variable_of_module_name ~loc:hd.location module_binder in
        let attr = known_attributes_for_modules (Some module_attr) in
        let env' = Mod_env.add mod_env (module_binder,ty) in
        aggregate (binder, mod_as_record, attr) ~new_env:env'
      )
      | I.Module_alias { alias ; binders } -> (
        let alias' = variable_of_module_name ~loc:hd.location alias in
        let alias_ty = Mod_env.get_ty mod_env alias in
        let alias_var = O.e_a_variable alias' alias_ty in
        let access =
          let f : O.expression -> string -> O.expression =
            fun prev name ->
              let label = O.Label (label_of_module_name name) in
              let ty = Option.value ~default:(failwith "TODO") (O.get_record_field_type prev.type_expression label) in
              O.e_a_record_access prev label ty
          in
          List.fold_left (List.Ne.to_list binders) ~f ~init:alias_var
        in
        let attr = known_attributes_for_modules None in
        aggregate (alias', access, attr)
      )
    )
    | [] -> failwith "empty module : never happens ?"

and compile_types : I.type_expression -> O.type_expression =
  fun _ -> failwith "l"

and compile_expression : Mod_env.t -> I.expression -> O.expression =
  fun mod_env expr ->
    let self = compile_expression mod_env in
    let self_cases = compile_cases mod_env in
    let return expression_content : O.expression =
      let type_expression = compile_types expr.type_expression in
      { expression_content ; type_expression ; location = expr.location } in
    match expr.expression_content with
    | I.E_literal l ->
      return (O.E_literal l)
    | I.E_variable v ->
      return (O.E_variable v)
    | I.E_raw_code { language ; code } ->
      let code = self code in
      return (O.E_raw_code { language ; code })
    | I.E_matching {matchee=e;cases} -> (
      let e' = self e in
      let cases' = self_cases cases in
      return @@ O.E_matching {matchee=e';cases=cases'}
    )
    | I.E_record_accessor {record; path} -> (
      let record = self record in
      return @@ O.E_record_accessor {record; path}
    )
    | I.E_record m -> (
      let m' = O.LMap.map self m in
      return @@ O.E_record m'
    )
    | I.E_record_update {record; path; update} -> (
      let record = self record in
      let update = self update in
      return @@ O.E_record_update {record;path;update}
    )
    | I.E_constructor { constructor ; element } -> (
      let element = self element in
      return @@ O.E_constructor { constructor ; element }
    )
    | I.E_application {lamb; args} -> (
      let ab = (lamb, args) in
      let (a,b) = Pair.map ~f:self ab in
      return @@ O.E_application {lamb=a;args=b}
    )
    | I.E_let_in { let_binder ; rhs ; let_result; attr } -> (
      let rhs = self rhs in
      let let_result = self let_result in
      return @@ O.E_let_in { let_binder ; rhs ; let_result; attr }
    )
    | I.E_type_in {type_binder; rhs; let_result} -> (
      let let_result = self let_result in
      let rhs = compile_types rhs in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    )
    | I.E_lambda { binder ; result } -> (
      let result = self result in
      return @@ O.E_lambda { binder ; result }
    )
    | I.E_type_inst { forall ; type_ } -> (
      let forall = self forall in
      let type_ = compile_types type_ in
      return @@ O.E_type_inst { forall ; type_ }
    )
    | I.E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let result = self result in
      let fun_type = compile_types fun_type in 
      return @@ O.E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
    | I.E_constant { cons_name ; arguments } -> (
      let arguments = List.map ~f:self arguments in
      return @@ O.E_constant { cons_name ; arguments }
    )
    | I.E_module_accessor { module_name; element } -> (
      ignore module_name; ignore element;
      failwith "nope"
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let let_binder = Location.wrap @@ Var.of_name module_binder in
      let rhs, rhs_ty = module_to_record mod_env rhs in
      let rhs = O.make_e (O.E_record rhs) rhs_ty in
      let env' = Mod_env.add mod_env (module_binder, rhs_ty) in
      let let_result = compile_expression env' let_result in
      return @@ O.e_let_in let_binder rhs let_result (known_attributes_for_modules None)
    )
    | I.E_mod_alias { alias ; binders ; result } -> (
      ignore alias; ignore binders; ignore result;
      failwith "nope2"
    )

and compile_cases : Mod_env.t -> I.matching_expr -> O.matching_expr =
  fun mod_env m ->
    match m with
    | Match_variant {cases;tv} -> (
        let aux { I.constructor ; pattern ; body } =
          let body = compile_expression mod_env body in
          {O.constructor;pattern;body}
        in
        let cases = List.map ~f:aux cases in
        let tv = compile_types tv in
        Match_variant {cases ; tv}
      )
    | Match_record {fields; body; tv} ->
      let body = compile_expression mod_env body in
      let tv = compile_types tv in
      let fields = O.LMap.map (fun (v, t) -> (v, compile_types t)) fields in
      Match_record {fields; body; tv}

and module_to_record : Mod_env.t -> I.module_fully_typed -> O.expression_label_map * O.type_expression = (* Label x -> (exp : ty) *)
  fun mod_env (Module_Fully_Typed lst) ->
    let aux acc (m : I.declaration_loc) = match m.wrap_content with
      | I.Declaration_constant { name = _ ; binder ; expr ; _ } ->
        let expr = compile_expression mod_env expr in
        O.LMap.add (Label (Var.to_name binder.wrap_content)) expr acc
      | I.Declaration_type _ -> acc
      | I.Declaration_module {module_binder ; module_ ; _ } ->
        let (content, ty) = module_to_record mod_env module_ in
        let expr = O.make_e (O.E_record content) ty in
        O.LMap.add (Label module_binder) expr acc
      | I.Module_alias { alias ; binders } ->
        let alias' = variable_of_module_name ~loc:Location.generated alias in
        let alias_ty = Mod_env.get_ty mod_env alias in
        let alias_var = O.e_a_variable alias' alias_ty in
          let f : O.expression -> string -> O.expression =
            fun prev name ->
              let label = O.Label (label_of_module_name name) in
              let ty = Option.value ~default:(failwith "TODO") (O.get_record_field_type prev.type_expression label) in
              O.e_a_record_access prev label ty
          in
        let expr = List.fold_left (List.Ne.to_list binders) ~f ~init:alias_var in
        O.LMap.add (Label alias) expr acc
    in
    let env = List.fold ~init:O.LMap.empty ~f:aux lst in
    let ty = type_of_expression_label_map env in
    env, ty
    (* do not return an expression because I would like to wrap the content into E_record in compile_declaration *)

and type_of_expression_label_map env =
  O.make_t_ez_record (List.map ~f:(fun (O.Label s, v) -> (s, v.type_expression)) (O.LMap.to_kv_list env))

and label_of_module_name x = x
and variable_of_module_name : loc:Location.t -> string -> O.expression_variable =
  fun ~loc str -> Location.wrap ~loc @@ Var.fresh_like (Var.of_name str)

and known_attributes_for_modules : I.module_attribute option -> O.known_attributes = fun opt ->
  let default : O.known_attributes = (* This should be defined somewhere ? *)
    { inline = false ; no_mutation = false ; view = false ; public = false ; }
  in
  Option.value_map opt ~default ~f:(fun {public} -> { default with public})