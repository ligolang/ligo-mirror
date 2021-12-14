module I = Ast_typed
module O = Ast_aggregated
open Stage_common.Types
open Trace
type err = Errors.aggregation_error raise

(* this pass does the following:
  - aggregates declarations into a chain of let-ins
  - transform modules to "top-level" definitions
*)
(* let name_of_path : string list -> string =
  fun lst ->
    List.fold lst  *)

module Data = struct
  type uniq_ident = int
  type path_id = (module_variable * uniq_ident)
  type path = path_id list
  type env_item =
    | Expression of { name: expression_variable ; item: Ast_aggregated.expression  }
    | Module of { name: path_id ; item: module_env }

  and module_env = env_item list

  type t = {
    curr_path : path ;
    env : module_env ;
  }

  let rec pp_module_env : Format.formatter -> module_env -> unit = fun ppf env ->
    let aux : Format.formatter -> env_item -> unit = fun ppf ->
      function | Expression {name;item} -> ignore item ;
                  Format.fprintf ppf "%a -> XX" Var.pp name.wrap_content (*O.PP.expression item *)
               | Module {name = (name,id);item} ->
                  Format.fprintf ppf "(%a, %d) -> %a" Ast_typed.PP.module_variable name id pp_module_env item in
    Format.fprintf ppf "@[<h>env:[%a]@]"
      (* (List.length env) *)
      PP_helpers.(list_sep aux (tag ","))
      env
  let pp_data : Format.formatter -> t -> unit = fun ppf data ->
    Format.fprintf ppf "@[<h>curr_path: [%a]@.%a@]" (PP_helpers.list_sep_d (PP_helpers.pair PP_helpers.string PP_helpers.int)) data.curr_path pp_module_env data.env
  let empty = { curr_path = [] ; env = [] }
  let path_equal : path_id -> path_id -> bool = fun (n1,i1) (n2,i2) -> String.equal n1 n2 && Int.equal i1 i2

  let name_in_current_path data (name: O.expression_variable) =
    let loc = name.location in
    let (name,_) = Var.internal_get_name_and_counter name.wrap_content in
    let str : string = List.fold_right ~f:(fun (s,_) r -> s ^ "#" ^ r) ~init:name data.curr_path in
    Location.wrap ~loc (Var.of_name str)

  let prefix_var prefix (name: O.expression_variable) =
    let loc = name.location in
    let (name,_) = Var.internal_get_name_and_counter name.wrap_content in
    let str : string = List.fold_right ~f:(fun s r -> s ^ "#" ^ r) ~init:name prefix in
    Location.wrap ~loc (Var.of_name str)

  let modules : module_env -> (path_id * module_env) list = fun env ->
    List.filter_map env ~f:(function | Module {name;item} -> Some (name, item) | Expression _ -> None)

  let resolve_module_path : t -> module_variable List.Ne.t -> module_env =
    fun data requested_path ->
      let requested_path = List.Ne.to_list requested_path in
      let f (e: module_env) (m : module_variable) =
        let modules_cur_level = modules e in
        let last_of_path =
          List.last data.curr_path
        in
        match last_of_path with
        | Some (_,i) -> (
          let lst = List.filter modules_cur_level ~f:(fun ((a,_),_) -> String.equal a m) in
          if List.length lst > 1 then (
            match List.Assoc.find modules_cur_level ~equal:path_equal (m,i-1) with
            | None -> failwith (Format.asprintf "1coner case: could not resolve module alias rhs. This shouldn't pass typechecking: %s" m)
            | Some e -> e
          ) else (
            match List.Assoc.find modules_cur_level ~equal:(fun (a,_) (b,_) -> String.equal a b) (m,-42) with
            | None -> failwith (Format.asprintf "2coner case: could not resolve module alias rhs. This shouldn't pass typechecking: %s" m)
            | Some e -> e
          )
        )
        | None -> (
          let lst = List.filter modules_cur_level ~f:(fun ((a,_),_) -> String.equal a m) in
          let lst = List.max_elt lst ~compare:(fun ((_,a),_) ((_,b),_) -> Int.compare a b) in 
          (* let () = Format.eprintf "\n\n!!\n%a\n%a\n" pp_data data (PP_helpers.list_sep_d PP_helpers.string) requested_path in *)
          match lst with
          | Some (_,x) -> x
          | None -> failwith (Format.asprintf "coner case: could not resolve module alias rhs. This shouldn't pass typechecking: %s" m)
        )
      in
      List.fold_left ~f ~init:data.env requested_path

  let rec add_to_module : module_env -> path -> env_item -> module_env = fun env path new_item ->
    match path with
    | [] -> new_item :: env
    | m :: path -> (
      let f =
        function
        | Module { name ; item } when path_equal name m ->
          let item = add_to_module item path new_item in
          Some (Module { name ; item })
        | _ -> None
      in
      List.rev @@ List.update_first (List.rev env) ~f
    )
  let enter_new_module : t -> module_variable -> t = fun data name ->
    (* update the current path and create a new empty module in the environment *)
    let cur_env = match data.curr_path with
      | [] -> data.env
      | _ -> resolve_module_path data (List.Ne.of_list (List.map ~f:fst data.curr_path))
    in
    let i = List.count cur_env ~f:(function Module { name = (name',_); _ } when String.equal name name' -> true | _ -> false) in
    let env = add_to_module data.env data.curr_path (Module {name = (name,i); item = [] }) in
    let curr_path = data.curr_path @ [(name,i)] in
    { env ; curr_path }
  let extend_expression : t -> expression_variable -> Ast_aggregated.expression -> t = fun data name term ->
    { data with env = add_to_module data.env data.curr_path (Expression {name ; item = term }) }
  let add_alias : t -> module_variable -> module_env -> t = fun data name item ->
    let cur_env = match data.curr_path with
      | [] -> data.env
      | _ -> resolve_module_path data (List.Ne.of_list (List.map ~f:fst data.curr_path))
    in
    let i = List.count cur_env ~f:(function Module { name = (name',_); _ } when String.equal name name' -> true | _ -> false) in
    let env = add_to_module data.env data.curr_path (Module {name = (name,i); item }) in
    { data with env }
  let resolve_variable : t -> expression_variable -> expression_variable option =
    fun data v ->
      let f = function
        | Expression { name ; _ } when Var.equal name.wrap_content v.wrap_content ->
          Some (name_in_current_path data name)
        | (Module _ | Expression _) -> None
      in
      let res_env = match data.curr_path with
      | [] -> data.env
      | _ -> resolve_module_path data (List.Ne.of_list (List.map ~f:fst data.curr_path)) in
      List.find_map res_env ~f

  let resolve_variable_in_path : t -> module_variable list -> expression_variable -> expression_variable option =
    fun data path v ->
      (* print_endline (Format.asprintf "data: %a" pp_data data); *)
      let f = function
        | Expression { name ; _ } when Var.equal name.wrap_content v.wrap_content ->
          Some (prefix_var path name)
        | (Module _ | Expression _) -> None
      in
      let res_env = match path with
        | [] -> data.env
        | _ -> resolve_module_path data (List.Ne.of_list path)
      in
      match List.find_map res_env ~f with
      | None ->
         let path = (List.map ~f:fst data.curr_path) @ path in
         let res_env = match path with
           | [] -> data.env
           | _ -> resolve_module_path data (List.Ne.of_list path) in
         List.find_map res_env ~f
      | Some v -> Some v

end

type result =
  | Hole of I.expression (* the hole as the expression  *)
  | Rec of O.expression (* the hole given when calling reccursively *)

let rec compile ~raise : I.expression -> I.module_fully_typed -> O.expression =
  fun hole (Module_Fully_Typed module_) ->
    let f , _ = compile_declaration ~raise Data.empty module_ in
    f ~hole:(Hole hole)

and compile_declaration ~raise : Data.t -> I.declaration_loc list -> (hole:result -> O.expression) * Data.t =
  fun data lst ->
    match lst with
    | hd::tl -> (
      (* let () = Format.eprintf "\nCompile_declaration '%a'\n" I.PP.declaration hd.wrap_content in
       * let () = Format.eprintf "%a\n" Data.pp_data data in *)
      let skip () = compile_declaration ~raise data tl in
      match hd.wrap_content with
      | I.Declaration_type _ -> skip ()
      | I.Declaration_constant { name = _ ; binder ; expr ; attr } -> (
        let expr = compile_expression ~raise data expr in
        let data = Data.extend_expression data binder expr in
        let f, data = compile_declaration ~raise data tl in
        (fun ~hole ->
          O.e_a_let_in (Data.name_in_current_path data binder) expr (f ~hole) attr), data
      )
      | I.Declaration_module { module_binder ; module_ ; module_attr = _ } -> (
        let (Module_Fully_Typed decls) = module_ in
        let data' = Data.enter_new_module data module_binder in
        let f, data_mod = compile_declaration ~raise data' decls in
        let f_rest , data_rest = compile_declaration ~raise { data_mod with curr_path = data.curr_path } tl in
        (fun ~hole ->
          let hold_rest = Rec (f_rest ~hole) in
          f ~hole:hold_rest), data_rest
      )
      | I.Module_alias { alias ; binders } -> (
        let rec aux = fun prefix acc el ->
          match el with
          | Data.Expression {name ; item} ->
            O.e_a_let_in (Data.prefix_var prefix name) item acc (known_attributes_for_modules None)
          | Data.Module {name=(name,_) ; item} -> (
            List.fold item ~f:(aux (prefix@[name])) ~init:acc
          )
        in
        let env : Data.module_env = Data.resolve_module_path data binders in
        let data = Data.add_alias data alias env in
        let (f, data) = compile_declaration ~raise data tl in
        (fun ~hole -> List.fold env ~f:(aux ((List.map ~f:fst data.curr_path) @ [alias])) ~init:(f ~hole)), data
      )
    )
    | [] -> (
      (fun ~hole ->
        match hole with
        | Rec x -> x
        | Hole x -> compile_expression ~raise data x
      ), data
    )

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

and compile_expression ~raise : Data.t -> I.expression -> O.expression =
  fun data expr ->
    (* let () = Format.eprintf "compile_expression '%a'\n" I.PP.expression expr in
     let () = Format.eprintf "%a\n" Data.pp_data data in *)
    let self ?(data = data) = compile_expression ~raise data in
    let return expression_content : O.expression =
      let type_expression = compile_type ~raise expr.type_expression in
      { expression_content ; type_expression ; location = expr.location } in
    match expr.expression_content with
    | I.E_literal l ->
      return (O.E_literal l)
    | I.E_variable v -> (
      match Data.resolve_variable data v with
      | None -> return (O.E_variable v)
      | Some prefixed_var -> return (O.E_variable prefixed_var)
    )
    | I.E_raw_code { language ; code } ->
      let code = self code in
      return (O.E_raw_code { language ; code })
    | I.E_matching {matchee=e;cases} -> (
      let e' = self e in
      let cases' = compile_cases ~raise data cases in
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
      let rec aux : string List.Ne.t -> (O.type_expression * O.type_expression) list -> I.expression -> _ * string List.Ne.t * (O.type_expression * O.type_expression) list * _ option =
        fun acc_path acc_types exp ->
          match exp.expression_content with
          | E_module_accessor {module_name ; element} ->
            let acc_path = Simple_utils.List.Ne.cons module_name acc_path in
            aux acc_path acc_types element
          | E_type_inst { forall ; type_ } ->
            let type_ = compile_type ~raise type_ in
            let exp_ty = compile_type ~raise exp.type_expression in
            aux acc_path ((type_, exp_ty) :: acc_types) forall
          | E_variable v ->
            v, acc_path, acc_types, None
          | E_record_accessor _ ->
            let rec aux' (e : I.expression) acc_path = match e.expression_content with
              | E_variable v ->
                v, acc_path
              | E_record_accessor { record ; path } ->
                aux' record ((path, compile_type ~raise e.type_expression) :: acc_path)
              | _ -> failwith "oops" in
            let v, path = aux' exp [] in
            v, acc_path, acc_types, Some path
          | _ -> failwith "TODO: corner case, not allowed in the syntax"
      in
      let v, path, types, record_path = aux (List.Ne.of_list [module_name]) [] element in
      let path = List.Ne.rev path in
      let path = List.Ne.to_list path in
      (* print_endline (Format.asprintf "%a" I.PP.expression expr); *)
      match Data.resolve_variable_in_path data path v with
      | None -> failwith (Format.asprintf "%a | %a | %a" (PP_helpers.list_sep_d (PP_helpers.pair PP_helpers.string PP_helpers.int)) data.curr_path (PP_helpers.list_sep_d PP_helpers.string) path O.PP.expression_variable v);
      | Some v ->
         match record_path with
         | None ->
            (* module_access_to_record_access mod_env path types *)
            let expr = O.e_a_variable v (compile_type ~raise expr.type_expression) in
            List.fold_right ~f:(fun (t, u) e -> O.e_a_type_inst e t u) ~init:expr (List.rev types)
         | Some record_path ->
            let expr = O.e_a_variable v (compile_type ~raise expr.type_expression) in
            let expr = List.fold_right ~f:(fun (l, t) r -> O.e_a_record_access r l t) ~init:expr record_path in
            List.fold_right ~f:(fun (t, u) e -> O.e_a_type_inst e t u) ~init:expr (List.rev types)
    )
    | I.E_mod_in { module_binder ; rhs ; let_result } -> (
      let mod_decl = Location.wrap ~loc:expr.location @@
        I.Declaration_module { module_binder ; module_ = rhs ; module_attr = { public = true } }
      in
      let (f,data) = compile_declaration ~raise { data with curr_path = [] } [mod_decl] in
      let let_result = self ~data let_result in
      f ~hole:(Rec let_result)
    )
    | I.E_mod_alias { alias ; binders ; result } -> (
      let rec aux = fun prefix acc el ->
        match el with
        | Data.Expression {name ; item} ->
          O.e_a_let_in (Data.prefix_var prefix name) item acc (known_attributes_for_modules None)
        | Data.Module {name = (name,_) ; item} -> (
          List.fold item ~f:(aux (prefix@[name])) ~init:acc
        )
      in
      let env : Data.module_env = Data.resolve_module_path data binders in
      let result = self ~data result in
      List.fold env ~f:(aux ((List.map ~f:fst data.curr_path) @ [alias])) ~init:result
    )

and compile_cases ~raise : Data.t -> I.matching_expr -> O.matching_expr =
  fun data m ->
    match m with
    | Match_variant {cases;tv} -> (
        let aux { I.constructor ; pattern ; body } =
          let body = compile_expression ~raise data body in
          {O.constructor;pattern;body}
        in
        let cases = List.map ~f:aux cases in
        let tv = compile_type ~raise tv in
        Match_variant {cases ; tv}
      )
    | Match_record {fields; body; tv} ->
      let fields = O.LMap.map (fun (v, t) -> (v, compile_type ~raise t)) fields in
      let body = compile_expression ~raise data body in
      let tv = compile_type ~raise tv in
      Match_record {fields; body; tv}

and known_attributes_for_modules : I.module_attribute option -> O.known_attributes = fun opt ->
  let default : O.known_attributes = (* This should be defined somewhere ? *)
    { inline = false ; no_mutation = false ; view = false ; public = false ; }
  in
  Option.value_map opt ~default ~f:(fun {public} -> { default with public})

(* and module_path_to_lident (mod_env : Data.t) (l : module_variable Simple_utils.List.Ne.t) : O.expression_variable =
  let (hd, tl) = l in
  let prefix = match Var_env.find_mod hd mod_env with
    | None -> hd
    | Some l -> fst (Var.internal_get_name_and_counter l.wrap_content)
  in
  let s = List.fold_left ~f:(fun n r -> n ^ "_" ^ r) ~init:prefix tl in
  Location.wrap @@ Var.of_name s *)
