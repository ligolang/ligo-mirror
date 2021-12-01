module I = Ast_typed
module O = Ast_aggregated
open Stage_common.Types
open Trace
type err = Errors.aggregation_error raise

(* this pass does the following:
  - aggregates declarations into a chain of let-ins
  - unify modules and records
*)

type binder_repr = O.expression_variable

module Mod_env = struct
  let variable_of_module_name : loc:Location.t -> string -> O.expression_variable =
    fun ~loc str -> Location.wrap ~loc @@ Var.fresh_like (Var.of_name str)
  module ModMap = Map.Make(struct type t = string let compare = String.compare end)
  type t = (O.row_element label_map * binder_repr) ModMap.t
  let find : t -> string -> O.type_expression * binder_repr = fun env name ->
    match ModMap.find_opt name env with
    | Some (content,b) -> (O.t_record ~layout:I.default_layout content,b)
    | None -> failwith (Format.asprintf "TODO: corner case : module '%s' is missing" name)
  let find_opt : t -> string -> (O.type_expression * binder_repr) option = fun env name ->
    match ModMap.find_opt name env with
    | Some (content,b) -> Some (O.t_record ~layout:I.default_layout content,b)
    | None -> None
  let add ~loc : t -> string * O.type_expression -> t * O.expression_variable =
    fun env (name, t) ->
      let ty = O.get_t_record_exn t in
      let var = variable_of_module_name ~loc name in
      ModMap.add name (ty.content, var) env, var
  let empty = ModMap.empty
end

let rec compile ~raise : I.expression -> I.module_fully_typed -> O.expression =
  (*TODO*) ignore raise;
  fun hole (Module_Fully_Typed module_) -> compile_declaration ~raise ~hole Mod_env.empty module_

and compile_declaration ~raise : hole:I.expression -> Mod_env.t -> I.declaration_loc list -> O.expression =
  fun ~hole mod_env lst ->
    match lst with
    | hd::tl -> (
      let aggregate ?(new_env = mod_env) (binder, expr, attr) = O.e_a_let_in binder expr (compile_declaration ~raise ~hole new_env tl) attr in
      let skip () = compile_declaration ~raise ~hole mod_env tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip ()
      | I.Declaration_constant { name = _ ; binder ; expr ; attr } -> (
        let expr = compile_expression ~raise mod_env expr in
        aggregate (binder, expr, attr)
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr } -> (
        let mod_as_record =
          let (Module_Fully_Typed decls) = module_ in
          module_to_record ~raise mod_env decls in
        let attr = known_attributes_for_modules (Some module_attr) in
        let env',binder = Mod_env.add ~loc:hd.location mod_env (module_binder,mod_as_record.type_expression) in
        aggregate (binder, mod_as_record, attr) ~new_env:env'
      )
      | I.Module_alias { alias ; binders } -> (
        let access = module_access_to_record_access mod_env binders [] in
        let attr = known_attributes_for_modules None in
        let env',alias = Mod_env.add ~loc:hd.location mod_env (alias, access.type_expression) in
        aggregate (alias, access, attr) ~new_env:env'
      )
    )
    | [] -> compile_expression ~raise mod_env hole

(*
module_to_record mod_env ?(acc = []) lst
morph a module [lst] to a record assuming a module environment [mod_env]. All field are pre-declared in let-ins, e.g.
``` (I.expression ==> O.expression)
  module My_module = struct         let My_module#1 =
    let x = 1                         let x = 1 in
    module AA = struct                let AA#2 =
      let foo = 1                       let foo = 1 in
      let bar = 2                       let bar = 2 in
    end                                 {foo ; bar}
  end                                 in
  ...                                 { x = x ; AA = AA#2 }
  ...                               in
  ...                               ...
```
[acc] is used to propagate binder,labels and type up to the end when we generate the final record (here, `{ x = x ; AA = AA#1 }`)
*)
and module_to_record ~raise : Mod_env.t -> ?acc:(binder_repr * label * O.expression) list -> I.declaration_loc list -> O.expression =
  fun mod_env ?(acc = []) lst ->
    match lst with
    | hd::tl -> (
      let aggregate ?(new_env = mod_env) ((binder,name_opt), expr, attr) =
        let label = match name_opt with
          | Some name -> Label name
          | None -> label_for_constant_decl binder
        in
        let acc = (binder,label,expr)::acc in
        O.e_a_let_in binder expr (module_to_record ~raise ~acc new_env tl) attr
      in
      let skip () = module_to_record ~raise ~acc mod_env tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip ()
      | I.Declaration_constant { name ; binder ; expr ; attr } -> (
        let expr = compile_expression ~raise mod_env expr in
        aggregate ((binder,name), expr, attr)
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr } -> (
        let mod_as_record =
          let (Module_Fully_Typed decls) = module_ in
          module_to_record ~raise mod_env decls
        in
        let attr = known_attributes_for_modules (Some module_attr) in
        let mod_env,binder = Mod_env.add ~loc:hd.location mod_env (module_binder,mod_as_record.type_expression) in
        aggregate ((binder,Some module_binder), mod_as_record, attr) ~new_env:mod_env
      )
      | I.Module_alias { alias ; binders } -> (
        let access = module_access_to_record_access mod_env binders [] in
        let attr = known_attributes_for_modules None in
        let mod_env,binder = Mod_env.add ~loc:hd.location mod_env (alias, access.type_expression) in
        aggregate ((binder,Some alias), access, attr) ~new_env:mod_env
      )
    )
    | [] -> record_of_binders acc

and compile_type ~raise : Mod_env.t -> I.type_expression -> O.type_expression =
  fun mod_env ty ->
    let self = compile_type ~raise mod_env in
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

and compile_expression ~raise : Mod_env.t -> I.expression -> O.expression =
  fun mod_env expr ->
    let self = compile_expression ~raise mod_env in
    let self_cases = compile_cases ~raise mod_env in
    let return expression_content : O.expression =
      let type_expression = compile_type ~raise mod_env expr.type_expression in
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
      let rhs = compile_type ~raise mod_env rhs in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    )
    | I.E_lambda { binder ; result } -> (
      let result = self result in
      return @@ O.E_lambda { binder ; result }
    )
    | I.E_type_inst { forall ; type_ } -> (
      let forall = self forall in
      let type_ = compile_type ~raise mod_env type_ in
      return @@ O.E_type_inst { forall ; type_ }
    )
    | I.E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let result = self result in
      let fun_type = compile_type ~raise mod_env fun_type in
      return @@ O.E_recursive { fun_name; fun_type; lambda = {binder;result}}
    )
    | I.E_constant { cons_name ; arguments } -> (
      let arguments = List.map ~f:self arguments in
      return @@ O.E_constant { cons_name ; arguments }
    )
    | I.E_module_accessor { module_name; element} -> (
      let rec aux : string List.Ne.t -> (O.type_expression * O.type_expression) list -> I.expression -> string List.Ne.t * (O.type_expression * O.type_expression) list =
        fun acc_path acc_types exp ->
          match exp.expression_content with
          | E_module_accessor {module_name ; element} ->
            let acc_path = Simple_utils.List.Ne.cons module_name acc_path in
            aux acc_path acc_types element
          | E_variable v ->
            let (name,_int_opt) = Var.internal_get_name_and_counter v.wrap_content in (* feels wrong *)
            Simple_utils.List.Ne.cons name acc_path, acc_types
          | E_type_inst { forall ; type_ } ->
            let type_ = compile_type ~raise mod_env type_ in
            let exp_ty = compile_type ~raise mod_env exp.type_expression in
            aux acc_path ((type_, exp_ty) :: acc_types) forall
          | _ -> failwith "TODO: corner case, not allowed in the syntax"
      in
      let path, types = aux (List.Ne.of_list [module_name]) [] element in
      let path = List.Ne.rev path in
      module_access_to_record_access mod_env path types
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let mod_as_record =
        let (Module_Fully_Typed decls) = rhs in
        module_to_record ~raise mod_env decls
      in
      let env',binder = Mod_env.add ~loc:Location.generated mod_env (module_binder, mod_as_record.type_expression) in
      let let_result = compile_expression ~raise env' let_result in
      return @@ O.e_let_in binder mod_as_record let_result (known_attributes_for_modules None)
    )
    | I.E_mod_alias { alias ; binders ; result } -> (
      let access = module_access_to_record_access mod_env binders [] in
      let (env',alias) = Mod_env.add ~loc:Location.generated mod_env (alias, access.type_expression) in
      let result = compile_expression ~raise env' result in
      let attr = known_attributes_for_modules None in
      return @@ O.e_let_in alias access result attr
    )

and compile_cases ~raise : Mod_env.t -> I.matching_expr -> O.matching_expr =
  fun mod_env m ->
    match m with
    | Match_variant {cases;tv} -> (
        let aux { I.constructor ; pattern ; body } =
          let body = compile_expression  ~raise mod_env body in
          {O.constructor;pattern;body}
        in
        let cases = List.map ~f:aux cases in
        let tv = compile_type ~raise mod_env tv in
        Match_variant {cases ; tv}
      )
    | Match_record {fields; body; tv} ->
      let body = compile_expression ~raise mod_env body in
      let tv = compile_type ~raise mod_env tv in
      let fields = O.LMap.map (fun (v, t) -> (v, compile_type ~raise mod_env t)) fields in
      Match_record {fields; body; tv}

(* morph a module access (A.B.C) to a record access (a.b.c) *)
and module_access_to_record_access : Mod_env.t -> string List.Ne.t -> (O.type_expression * O.type_expression) list -> O.expression = fun mod_env (fst,path) _types ->
  let f : O.expression -> string -> O.expression =
    fun prev module_name ->
      let label = Label module_name in
      let ty = match O.get_record_field_type prev.type_expression label with
        | Some ty -> ty | None -> failwith "TODO: corner case, no field x ?"
      in
      O.e_a_record_access prev label ty
  in
  let (t,binder) = Mod_env.find mod_env fst in
  let init = O.e_a_variable binder t in
  let expr = List.fold_left path ~f ~init in
  List.fold_right ~f:(fun (t, u) e -> O.e_a_type_inst e t u) ~init:expr (List.rev _types)

and record_of_binders : (binder_repr * label *  O.expression) list -> O.expression =
  fun lst ->
    let type_of_rows : O.expression label_map -> O.type_expression =
      fun x -> O.make_t_ez_record (List.map ~f:(fun (O.Label s, v) -> (s, v.type_expression)) (O.LMap.to_kv_list x))
    in
    let f = fun (k,label,e : binder_repr * label * O.expression) ->
      let var = O.make_e (O.e_variable k) (e.type_expression) in
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
    Label name
