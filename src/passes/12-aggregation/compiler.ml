module I = Ast_typed
module O = Ast_aggregated
open Stage_common.Types

(* README
  aggregates declarations : Declaration_constant -> E_let_in
  morph modules : Declaration_module -> E_let_in (E_record ..)
*)

type binder_repr = {
  var : O.expression_variable ; (* binder given to a module when it is aggregated in a let-in *)
  label : label                 (* label the module should have if nested into another module *)
}

module Mod_env = struct
  (* env used to remember module types as they are morphed into modules *)
  let variable_of_module_name : loc:Location.t -> string -> O.expression_variable =
    fun ~loc str -> Location.wrap ~loc @@ Var.fresh_like (Var.of_name str)
  let label_of_module_name : string -> label =
    fun str -> Label ("m#"^str)
  module ModMap = Map.Make(struct type t = string let compare = String.compare end)
  type t = (O.row_element label_map * binder_repr) ModMap.t
  let get_ty : t -> string -> O.type_expression = fun env name ->
    match ModMap.find_opt name env with
    | Some (content,_) -> O.t_record ~layout:I.default_layout content
    | None -> failwith (Format.asprintf "no module 2 %s" name)
  let get_mod_binder : t -> string -> binder_repr = fun env name ->
    match ModMap.find_opt name env with
    | Some (_,b) -> b
    | None -> failwith (Format.asprintf "no module 1 %s" name)
  let add ~loc : t -> string * O.type_expression -> t * O.expression_variable =
    fun env (name, t) ->
      let ty = O.get_t_record_exn t in
      let var = variable_of_module_name ~loc name in
      let label = label_of_module_name name in
      let binder_repr = {var ; label} in
      ModMap.add name (ty.content, binder_repr) env, var
  let empty = ModMap.empty
end



let rec compile : I.module_fully_typed -> O.expression =
  fun (Module_Fully_Typed module_) -> compile_declaration Mod_env.empty module_

and compile_declaration : Mod_env.t -> I.declaration_loc list -> O.expression =
  fun mod_env lst ->
    match lst with
    | hd::tl -> (
      let aggregate ?(new_env = mod_env) (binder, expr, attr) = O.e_a_let_in binder expr (compile_declaration new_env tl) attr in
      let skip () = compile_declaration mod_env tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip ()
      | I.Declaration_constant { name = _ ; binder ; expr ; attr } -> (
        let expr = compile_expression mod_env expr in
        aggregate (binder, expr, attr)
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr } -> (
        let mod_as_record =
          let (Module_Fully_Typed decls) = module_ in
          module_to_record mod_env decls in
        let attr = known_attributes_for_modules (Some module_attr) in
        let env',binder = Mod_env.add ~loc:hd.location mod_env (module_binder,mod_as_record.type_expression) in
        aggregate (binder, mod_as_record, attr) ~new_env:env'
      )
      | I.Module_alias { alias ; binders } -> (
        let access = module_access_to_record_access mod_env binders in
        let attr = known_attributes_for_modules None in
        let env',alias = Mod_env.add ~loc:hd.location mod_env (alias, access.type_expression) in
        aggregate (alias, access, attr) ~new_env:env'
      )
    )
    | [] -> O.e_a_unit (* TODO: how to represent "programs"? *)

and compile_type : Mod_env.t -> I.type_expression -> O.type_expression =
  fun mod_env ty ->
    let self = compile_type mod_env in
    let return type_content : O.type_expression = { type_content ; orig_var = ty.orig_var ; location = ty.location } in
    let map_rows : I.row_element label_map -> O.row_element label_map = fun rows ->
      let f : I.row_element -> O.row_element = fun row -> { row with associated_type = self row.associated_type} in
      LMap.map f rows
    in
    match ty.type_content with
    | T_variable x -> return (T_variable x)
    | T_constant { language ; injection ; parameters } ->
      let parameters = List.map parameters ~f:self in
      return (T_constant { language ; injection ; parameters })
    | T_sum { content ; layout } ->
      let content = map_rows content in
      return (T_sum { content ; layout })
    | T_record { content ; layout } ->
      let content = map_rows content in
      return (T_record { content ; layout })
    | T_arrow { type1 ; type2 } ->
      let type1 = self type1 in
      let type2 = self type2 in
      return (T_arrow { type1 ; type2 })
    | T_module_accessor _ -> failwith "module accessor types should not end up here"
    | T_singleton x -> return (T_singleton x)
    | T_abstraction { ty_binder ; kind ; type_ } ->
      let type_ = self type_ in
      return (T_abstraction { ty_binder ; kind ; type_ })
    | T_for_all { ty_binder ; kind ; type_ } ->
      let type_ = self type_ in
      return (T_for_all { ty_binder ; kind ; type_ })

and compile_expression : Mod_env.t -> I.expression -> O.expression =
  fun mod_env expr ->
    let self = compile_expression mod_env in
    let self_cases = compile_cases mod_env in
    let return expression_content : O.expression =
      let type_expression = compile_type mod_env expr.type_expression in
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
      let rhs = compile_type mod_env rhs in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    )
    | I.E_lambda { binder ; result } -> (
      let result = self result in
      return @@ O.E_lambda { binder ; result }
    )
    | I.E_type_inst { forall ; type_ } -> (
      let forall = self forall in
      let type_ = compile_type mod_env type_ in
      return @@ O.E_type_inst { forall ; type_ }
    )
    | I.E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let result = self result in
      let fun_type = compile_type mod_env fun_type in
      return @@ O.E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
    | I.E_constant { cons_name ; arguments } -> (
      let arguments = List.map ~f:self arguments in
      return @@ O.E_constant { cons_name ; arguments }
    )
    | I.E_module_accessor { module_name; element} -> (
      let ty = Mod_env.get_ty mod_env module_name in
      let binder = Mod_env.get_mod_binder mod_env module_name in
      let expr = O.e_a_variable binder.var ty in
      (* deal with the problem of chained module_accessor .. *)
      let not_good = Option.value ~default:(Location.wrap @@ Var.of_name "lolilol") @@ O.get_variable (self element) in
      return @@ O.E_record_accessor {record = expr ; path = Label (Var.to_name not_good.wrap_content)}
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let mod_as_record =
        let (Module_Fully_Typed decls) = rhs in
        module_to_record mod_env decls
      in
      let env',binder = Mod_env.add ~loc:Location.generated mod_env (module_binder, mod_as_record.type_expression) in
      let let_result = compile_expression env' let_result in
      return @@ O.e_let_in binder mod_as_record let_result (known_attributes_for_modules None)
    )
    | I.E_mod_alias { alias ; binders ; result } -> (
      let access = module_access_to_record_access mod_env binders in
      let (env',alias) = Mod_env.add ~loc:Location.generated mod_env (alias, access.type_expression) in
      let result = compile_expression env' result in
      let attr = known_attributes_for_modules None in
      return @@ O.e_let_in alias access result attr
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
        let tv = compile_type mod_env tv in
        Match_variant {cases ; tv}
      )
    | Match_record {fields; body; tv} ->
      let body = compile_expression mod_env body in
      let tv = compile_type mod_env tv in
      let fields = O.LMap.map (fun (v, t) -> (v, compile_type mod_env t)) fields in
      Match_record {fields; body; tv}


(* morph a module, to rows *)
and module_to_record : Mod_env.t -> ?acc:(binder_repr * O.expression) list -> I.declaration_loc list -> O.expression =
  fun mod_env ?(acc = []) lst ->
    match lst with
    | hd::tl -> (
      let aggregate ?(new_env = mod_env) ((binder: binder_repr), expr, attr) =
        let acc = (binder,expr)::acc in
        O.e_a_let_in binder.var expr (module_to_record ~acc new_env tl) attr
      in
      let skip () = module_to_record ~acc mod_env tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip ()
      | I.Declaration_constant { name = _ ; binder ; expr ; attr } -> (
        let expr = compile_expression mod_env expr in
        aggregate ({var = binder ; label = label_for_constant_decl binder}, expr, attr)
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr } -> (
        let mod_as_record =
          let (Module_Fully_Typed decls) = module_ in
          module_to_record mod_env decls
        in
        let attr = known_attributes_for_modules (Some module_attr) in
        let mod_env,binder = Mod_env.add ~loc:hd.location mod_env (module_binder,mod_as_record.type_expression) in
        let label = (Mod_env.get_mod_binder mod_env module_binder).label in (*REMITODO: ugh*)
        let binder = { var=binder ; label } in
        aggregate (binder, mod_as_record, attr) ~new_env:mod_env
      )
      | I.Module_alias { alias ; binders } -> (
        let access = module_access_to_record_access mod_env binders in
        let attr = known_attributes_for_modules None in
        let mod_env,binder = Mod_env.add ~loc:hd.location mod_env (alias, access.type_expression) in
        let label = (Mod_env.get_mod_binder mod_env alias).label in (*REMITODO: ugh*)
        let binder = {label ; var = binder} in
        aggregate (binder, access, attr) ~new_env:mod_env
      )
    )
    | [] -> record_of_binders acc

(* morph a module access (A.B.C) to a record access (a.b.c) *)
and module_access_to_record_access : Mod_env.t -> string Simple_utils.List.Ne.t -> O.expression = fun mod_env (fst,path) ->
  let f : O.expression -> string -> O.expression =
    fun prev module_name ->
      let {var=_;label} = Mod_env.get_mod_binder mod_env module_name in
      let ty = Option.value ~default:(failwith "TODO") (O.get_record_field_type prev.type_expression label) in
      O.e_a_record_access prev label ty
  in
  (* let init = O.e_a_variable (Mod_env.variable_of_module_name ~loc:Location.generated fst) (Mod_env.get_ty mod_env fst) in *)
  let init = O.e_a_variable (Mod_env.get_mod_binder mod_env fst).var (Mod_env.get_ty mod_env fst) in
  List.fold_left path ~f ~init

and record_of_binders : (binder_repr * O.expression) list -> O.expression =
  fun lst ->
    let type_of_rows : O.expression label_map -> O.type_expression =
      fun x -> O.make_t_ez_record (List.map ~f:(fun (O.Label s, v) -> (s, v.type_expression)) (O.LMap.to_kv_list x))
    in
    let f = fun (k,v : binder_repr * O.expression) ->
      let {var ; label} = k in
      let var = O.make_e (O.e_variable var) (v.type_expression) in
      (label,var)
    in
    let x = LMap.of_list (List.map lst ~f) in
    let content = O.E_record x in
    let type_expression = type_of_rows x in
    O.make_e content type_expression

and known_attributes_for_modules : I.module_attribute option -> O.known_attributes = fun opt ->
  let default : O.known_attributes = (* This should be defined somewhere ? *)
    { inline = false ; no_mutation = false ; view = false ; public = false ; }
  in
  Option.value_map opt ~default ~f:(fun {public} -> { default with public})

and label_for_constant_decl : O.expression_variable -> label =
  fun var ->
    let (name,_int_opt) = Var.internal_get_name_and_counter var.wrap_content in
    (* TODO: int_opt ... problem ?*)
    Label name