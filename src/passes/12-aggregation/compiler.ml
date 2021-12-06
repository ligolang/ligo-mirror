module I = Ast_typed
module O = Ast_aggregated
open Stage_common.Types
open Trace
type err = Errors.aggregation_error raise

(* this pass does the following:
  - aggregates declarations into a chain of let-ins
  - transform modules to "top-level" definitions
*)
let prepend : string -> O.expression_variable -> string = fun str v ->
  str ^ fst (Var.internal_get_name_and_counter v.wrap_content)
let postpend : string -> string -> string = fun str1 str2 ->
  str1 ^ "_" ^ str2
module Mod_env = struct
  module ModMap = Map.Make(struct type t = string let compare = String.compare end)
  
  type t = {
    prefixes : (O.expression_variable) ModMap.t ; 
    current_mod : string ; (* prefix on the module currently being compiled *)
  }

  let add : t -> string -> O.expression_variable -> t = fun x k v -> { x with prefixes = ModMap.add k v x.prefixes }
  let add_cur_path : t -> string -> t = fun x current_mod -> { x with current_mod }
  let empty = { prefixes = ModMap.empty ; current_mod = "" }
  let find s x = ModMap.find_opt s x.prefixes
  let cur_mod : t -> string = fun data -> data.current_mod 
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
      | I.Declaration_module { module_binder ; module_ ; module_attr = _ } -> (
        let (Module_Fully_Typed decls) = module_ in
        let lst =
          let mod_env = Mod_env.add_cur_path mod_env module_binder in
          module_to_record ~raise mod_env decls
        in
        let rest = compile_declaration ~raise ~hole mod_env tl in
        List.fold_right lst ~f:(fun (binder,expr,attr) acc -> O.e_a_let_in binder expr acc attr ) ~init:rest
      )
      | I.Module_alias { alias ; binders } -> (
        let env = Mod_env.add mod_env alias (module_path_to_lident mod_env binders) in
        compile_declaration ~raise ~hole env tl
      )
    )
    | [] -> compile_expression ~raise mod_env hole

and module_to_record ~raise : Mod_env.t -> I.declaration_loc list -> (O.expression_variable * O.expression * O.known_attributes) list =
  fun mod_env lst ->
    match lst with
    | hd::tl -> (
      let skip mod_env () = module_to_record ~raise mod_env tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip mod_env ()
      | I.Declaration_constant { name = _ ; binder ; expr ; attr } -> (
        let expr = compile_expression ~raise mod_env expr in
        let binder = Location.wrap ~loc:hd.location @@ Var.of_name (prepend (Mod_env.cur_mod mod_env) binder) in
        (binder,expr,attr)::(module_to_record ~raise mod_env tl)
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr = _ } -> (
        let mod_as_record =
          let (Module_Fully_Typed decls) = module_ in
          let mod_env = Mod_env.add_cur_path mod_env module_binder in
          module_to_record ~raise mod_env decls
        in
        mod_as_record @ (module_to_record ~raise mod_env tl)
      )
      | I.Module_alias { alias ; binders } -> (
        let lident = module_path_to_lident mod_env binders in (* binders => module_path_to_lident binders *)
        let env = Mod_env.add mod_env alias lident in
        module_to_record ~raise env tl
      )
    )
    | [] -> []

and compile_type ~raise : I.type_expression -> O.type_expression =
  fun ty ->
    let self = compile_type ~raise in
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
      let type_expression = compile_type ~raise expr.type_expression in
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
      let rhs = compile_type ~raise rhs in
      return @@ O.E_type_in {type_binder; rhs; let_result}
    )
    | I.E_lambda { binder ; result } -> (
      let result = self result in
      return @@ O.E_lambda { binder ; result }
    )
    | I.E_type_inst { forall ; type_ } -> (
      let forall = self forall in
      let type_ = compile_type ~raise type_ in
      return @@ O.E_type_inst { forall ; type_ }
    )
    | I.E_recursive { fun_name; fun_type; lambda = {binder;result}} -> (
      let result = self result in
      let fun_type = compile_type ~raise fun_type in
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
            let type_ = compile_type ~raise type_ in
            let exp_ty = compile_type ~raise exp.type_expression in
            aux acc_path ((type_, exp_ty) :: acc_types) forall
          | _ -> failwith "TODO: corner case, not allowed in the syntax"
      in
      let path, types = aux (List.Ne.of_list [module_name]) [] element in
      let path = List.Ne.rev path in
      (* module_access_to_record_access mod_env path types *)
      let expr = O.e_a_variable (module_path_to_lident mod_env path) (compile_type ~raise expr.type_expression) in
      List.fold_right ~f:(fun (t, u) e -> O.e_a_type_inst e t u) ~init:expr (List.rev types) 
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let lst =
        let (Module_Fully_Typed decls) = rhs in
        module_to_record ~raise mod_env decls
      in
      let mod_env = Mod_env.add_cur_path mod_env module_binder in
      chain_let_in lst @@ compile_expression ~raise mod_env let_result
    )
    | I.E_mod_alias { alias ; binders ; result } -> (
      let lident = module_path_to_lident mod_env binders in (* binders => module_path_to_lident binders *)
      let mod_env = Mod_env.add mod_env alias lident in
      compile_expression ~raise mod_env result
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
        let tv = compile_type ~raise tv in
        Match_variant {cases ; tv}
      )
    | Match_record {fields; body; tv} ->
      let body = compile_expression ~raise mod_env body in
      let tv = compile_type ~raise tv in
      let fields = O.LMap.map (fun (v, t) -> (v, compile_type ~raise t)) fields in
      Match_record {fields; body; tv}

and chain_let_in = fun lst rest -> List.fold_right lst ~f:(fun (binder,expr,attr) acc -> O.e_a_let_in binder expr acc attr ) ~init:rest

and known_attributes_for_modules : I.module_attribute option -> O.known_attributes = fun opt ->
  let default : O.known_attributes = (* This should be defined somewhere ? *)
    { inline = false ; no_mutation = false ; view = false ; public = false ; }
  in
  Option.value_map opt ~default ~f:(fun {public} -> { default with public})

and module_path_to_lident (mod_env : Mod_env.t) (l : module_variable Simple_utils.List.Ne.t) : O.expression_variable =
  let (hd, tl) = l in
  let prefix = match Mod_env.find hd mod_env with
    | None -> hd
    | Some l -> fst (Var.internal_get_name_and_counter l.wrap_content)
  in
  let s = List.fold_left ~f:(fun r n -> r ^ "_" ^ n) ~init:prefix tl in
  Location.wrap @@ Var.of_name s