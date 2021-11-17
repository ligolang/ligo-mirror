open Errors
open Trace
open Function

module CST = Cst.Pascaligo
module AST = Ast_imperative

open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map ~f:snd tl)
let build_ins = ["Operator";"Test";"Tezos";"Crypto";"Bytes";"List";"Set";"Map";"Big_map";"Bitwise";"String";"Layout";"Option"]

open Predefined.Tree_abstraction.Pascaligo

let r_split = Location.r_split

let mk_var var = if String.compare var Var.wildcard = 0 then Var.fresh () else Var.of_name var

let compile_attributes : CST.attributes -> AST.attributes = fun attributes ->
  List.map ~f:(fst <@ r_split) attributes

let compile_selection : CST.selection -> 'a access * location = function
  | FieldName name ->
    let (name, loc) = r_split name in
    (Access_record name, loc)
  | Component comp ->
    let ((_,index), loc) = r_split comp in
    (Access_tuple index, loc)

let rec compile_type_expression ~raise : CST.type_expr -> AST.type_expression =
  fun te ->
    let self = compile_type_expression ~raise in
    match te with
    | T_Var v -> (
      let (v,loc) = r_split v in
      t_variable_ez ~loc v
    )
    | T_Cart { region ; value = (fst, _ ,rest) } -> (
      let loc = Location.lift region in
      let lst = List.map ~f:self (fst::npseq_to_list rest) in
      t_tuple ~loc lst
    )
    | T_Sum { region ; value } -> (
      let loc = Location.lift region in
      let attr = compile_attributes value.attributes in
      let cases = Utils.nsepseq_to_list value.variants in
      let f : CST.variant CST.reg -> string * AST.type_expression * string list =
        fun { value = {ctor ; args ; attributes} ; region } ->
          let _loc = Location.lift region in
          let t = Option.value ~default:(t_unit ()) (Option.map ~f:(self <@ snd) args) in
          let case_attr = compile_attributes attributes in
          (ctor.value, t, case_attr)
      in
      t_sum_ez_attr ~loc ~attr (List.map ~f cases)
    )
    | T_App app -> (
      let get_t_string_singleton_opt =
        function
        | CST.T_String s -> Some s.value
        | _ -> None
      in
      let get_t_int_singleton_opt = function
        | CST.T_Int x ->
          let (_,z) = x.value in
          Some z
        | _ -> None
      in
      let ((type_constant,args), loc) = r_split app in
      match type_constant with
      | T_Var { value = "michelson_or" as v ; _ } -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_string_singleton_opt b in
          let d' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_string_singleton_opt d in
          let a' = self a in
          let c' = self c in
          t_michelson_or ~loc a' b' c' d'
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v
      )
      | T_Var { value = "michelson_pair" as v ; _ } -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [a ; b ; c ; d ] -> (
          let b' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_string_singleton_opt b in
          let d' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_string_singleton_opt d in
          let a' = self a  in
          let c' = self c  in
          t_michelson_pair ~loc a' b' c' d'
        )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v
      )
      | T_Var { value = "sapling_state" as v ; _ } -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_state ~loc singleton
        )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v
      )
      | T_Var { value = "sapling_transaction" as v ; _ } -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ Raw.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_transaction ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v
      )
      | T_Var type_constant -> (
        let operator = Var.of_name type_constant.value in
        let lst = npseq_to_list args.value.inside in
        let lst = List.map ~f:self lst in
        t_app ~loc operator lst
      )
      | _ -> failwith "TODO: t_app in the AST should be able to take a type expression in"
    )
    | T_Fun { region ; value = (lhs , _ , rhs) } -> (
      let loc = Location.lift region in
      t_function ~loc (self lhs) (self rhs)
    )
    | T_ModPath { region ; value = { module_path ; selector = _ ; field } } -> (
      let _loc = Location.lift region in
      let field = self field in
      let f : CST.module_name -> AST.type_expression -> AST.type_expression =
        fun x prev ->
          let (module_name, access_loc) = r_split x in
          let loc = Location.cover access_loc prev.location in
          t_module_accessor ~loc module_name prev
      in
      List.fold_right ~init:field ~f (npseq_to_list module_path)
    )
    | T_Par { region ; value = { lpar = _ ; inside ; rpar = _} } -> (
      let loc = Location.lift region in
      let inside = self inside in
      { inside with location = loc }
    )
    | T_Record { region ; value = { kind = _loc ; enclosing = _ ; elements ; terminator = _ ; attributes } } -> (
      let elements = Utils.sepseq_to_list elements in
      let f : CST.field_decl CST.reg -> string * AST.type_expression * AST.attributes = fun decl -> 
        let (({field_name;field_type;attributes} : CST.field_decl), _loc) = r_split decl in
        let t = 
          match field_type with
          | None ->
            (* type punning: { x } -> { x : x }*)
            let (v , loc) = r_split field_name in
            t_variable_ez ~loc v
          | Some (_colon , x) -> self x
        in
        let attributes = compile_attributes attributes in
        (field_name.value , t ,attributes)
      in
      let lst = List.map ~f elements in
      t_record_ez_attr ~loc:(Location.lift region) ~attr:(compile_attributes attributes) lst
    )
    | T_Int _ | T_String _ -> raise.raise @@ unsupported_string_singleton te

let rec compile_expression ~raise : CST.expr -> AST.expr = fun e ->
  ignore (raise,e) ; failwith "TODO"

  and conv ~raise : ?const:bool -> CST.pattern -> AST.ty_expr AST.pattern =
  fun ?(const = false) p ->
    let self = conv ~raise ~const in
    let check_no_attributes lst = if not (List.is_empty lst) then failwith "TODO: WARNING, attributes ignored in patterns" in
    match p with
    | P_Var var -> (
      let (var,loc) = r_split var in
      let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
      let b =
        let var = Location.wrap ~loc @@ match var.variable.value with
          | "_" -> Var.fresh ()
          | var -> Var.of_name var
        in
        { var ; ascr = None ; attributes }
      in
      Location.wrap ~loc (P_var b)
    )
    | P_Tuple tuple -> (
      let (tuple, loc) = r_split tuple in
      let lst = npseq_to_ne_list tuple.inside in
      let patterns = List.Ne.to_list lst in
      let nested = List.map ~f:self patterns in
      match nested with (* (x) == x *)
      | [x] -> x
      | _ -> Location.wrap ~loc @@ P_tuple nested
    )
    | P_App constr_pattern -> (
      let ((constr,p_opt), loc) = r_split constr_pattern in
      let rec get_ctor : CST.pattern -> string option = function
        | P_Par x -> get_ctor x.value.inside
        | P_Var x when Base.Char.is_uppercase (String.get x.value.variable.value 0) -> Some x.value.variable.value
        | _ -> None
      in
      match get_ctor constr with
      | Some "Unit" -> Location.wrap ~loc @@ P_unit
      | Some label ->
        let pv_opt = match p_opt with
          | Some p -> self (CST.P_Tuple p)
          | None -> Location.wrap ~loc P_unit
        in
        Location.wrap ~loc @@ P_variant (Label label, pv_opt)
      | None -> failwith "TODO: error, pattern is not a constructor"
    )
    | P_List { region ; value = { elements ; attributes } } -> (
      let () = check_no_attributes attributes in
      let loc = Location.lift region in
      let elements = Utils.sepseq_to_list elements in
      let f : CST.pattern -> AST.type_expression AST.pattern -> AST.type_expression AST.pattern =
        fun x prev ->
          let p = self x in
          Location.wrap (P_list (Cons (p, prev)))
      in
      List.fold_right ~f ~init:(Location.wrap ~loc (P_list (List []))) elements        
    )
    | P_Cons { region ; value = (hd,_,tl) } -> (
      let loc = Location.lift region in
      let hd = self hd in
      let tl = self tl in
      Location.wrap ~loc (P_list (Cons (hd,tl)))
    )
    | P_Nil (x: _ CST.wrap) -> (
      let loc = Location.lift x#region in
      Location.wrap ~loc (P_list (List []))
    )
    | P_Par { region = _ ; value } -> (
      self value.inside
    )
    | P_Record record_pattern -> (
      let (inj,loc) = r_split record_pattern in
      let lst = Utils.sepseq_to_list inj.elements in
      let aux : CST.field_pattern CST.reg -> AST.label * AST.ty_expr AST.pattern =
        fun x ->
          let (field, _) = r_split x in
          match field with
          | Punned { pun ; attributes } ->
            let () = List.iter ~f:check_no_attributes [attributes] in
            let loc = Location.lift pun.region in
            let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
            let binder = { var = Location.wrap ~loc (Var.of_name pun.value) ; ascr = None ; attributes } in
            (AST.Label pun.value , Location.wrap ~loc (P_var binder))
          | Complete { field_lhs; field_rhs ; attributes ; _} ->
            let () = check_no_attributes attributes in
            (AST.Label field_lhs.value, self field_rhs)
      in
      let lst' = List.map ~f:aux lst in
      let (labels,patterns) = List.unzip lst' in
      Location.wrap ~loc (P_record (labels,patterns))
    )
    | P_Typed {region ; value = { pattern ; type_annot = (_,ty_expr) }} -> (
      let loc = Location.lift region in
      let p = self pattern in
      let ty_expr = compile_type_expression ~raise ty_expr in
      match p.wrap_content with
      | P_var x -> (
        Location.wrap ~loc (P_var {x with ascr = Some ty_expr})
      )
      | _ -> failwith "TODO: error, unsupported type annotation (only supported on variables)"
    )
    | P_ModPath _ | P_Bytes _ | P_Int _ | P_Nat _ | P_String _ -> raise.raise @@ unsupported_pattern_type p

and compile_matching_expr : type a . raise:'b raise -> (a-> AST.expression) -> a CST.case_clause CST.reg List.Ne.t -> (AST.expression, AST.ty_expr) AST.match_case list =
  fun ~raise compiler cases ->
    let aux (case : a CST.case_clause CST.reg) =
      let (case, _loc) = r_split case in
      let expr    = compiler case.rhs in
      (case.pattern, expr)
    in
    let cases = List.Ne.map aux cases in
    let cases : (CST.pattern * AST.expression) list = List.Ne.to_list cases in
    let aux : (CST.pattern * AST.expression) -> (AST.expression , AST.ty_expr) match_case =
      fun (raw_pattern, body) ->
        let pattern = conv ~raise ~const:true raw_pattern in
        { pattern ; body }
    in
    List.map ~f:aux cases

and compile_parameters ~raise : CST.parameters -> (AST.type_expression AST.binder) list = fun params ->
  let aux : CST.param_decl CST.reg -> AST.type_expression AST.binder = fun param ->
    let (param, _loc) = r_split param in
    match param.param_kind with
    | `Var _ ->
      let (var, loc) = r_split param.var in
      let var = Location.wrap ~loc @@ Var.of_name var.variable.value in
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) param.param_type in
      { var ; ascr ; attributes = Stage_common.Helpers.const_attribute }
    | `Const _ ->
      let (var, loc) = r_split param.var in
      let var = Location.wrap ~loc @@ Var.of_name var.variable.value in
      let ascr = Option.map ~f:(compile_type_expression ~raise  <@ snd) param.param_type in
      { var ; ascr ; attributes = Stage_common.Helpers.var_attribute }
  in
  let (params, _loc) = r_split params in
  let params = npseq_to_list params.inside in
  List.map ~f:aux params

and compile_instruction ~raise : ?next: AST.expression -> CST.instruction -> AST.expression  = fun ?next instruction ->
  let return expr =
    match next with
    | Some e -> e_sequence expr e
    | None -> expr
  in
  let compile_tuple_expression : CST.tuple_expr -> AST.expression = fun tuple_expr ->
    let (lst, loc) = r_split tuple_expr in
    let lst = List.map ~f:(compile_expression ~raise) @@ npseq_to_list lst.inside in
    match lst with
    | hd::[] -> hd
    | lst -> e_tuple ~loc lst
  in
  let compile_if_clause : ?next:AST.expression -> CST.test_clause -> AST.expression =
      fun ?next if_clause ->
        match if_clause with
        | ClauseInstr i -> compile_instruction ~raise ?next i
        | ClauseBlock { value = { statements ; _ } ; region = _ } -> compile_block ~raise ?next statements
  in
  (* let compile_path : CST.path -> _ = fun path ->
    match path with
      Name var ->
      let (var,loc) = r_split var in
      let str = e_variable_ez ~loc var in
      (str, var, [])
    | Path proj ->
      let (proj, loc) = r_split proj in
      let (var, loc_var) = r_split proj.struct_name in
      let path = List.map ~f:compile_selection @@ npseq_to_list proj.field_path in
      let (path, _) = List.unzip path in
      let str = e_accessor ~loc (e_variable_ez ~loc:loc_var var) path in
      (str, var, path)
  in *)
  (* let compile_lhs : CST.lhs -> _ = fun lhs ->
    match lhs with
    | Path path ->
      let (_, var, path) = compile_path path in
      (var, path)
    | MapPath (mlu) ->
      let (mlu, _loc) = r_split mlu in
      let (_, var, path) = compile_path mlu.path in
      let index = compile_expression ~raise @@ mlu.index.value.inside in
      (var, path @ [Access_map index])
  in *)
  match instruction with
  | I_Cond { region ; value = { test ; if_so ; if_not } } ->
    let loc = Location.lift region in
    let test = compile_expression ~raise test in
    let ifso = compile_if_clause if_so in
    let ifnot =
      match if_not with
      | Some (_, not_case) -> compile_if_clause not_case
      | None -> e_skip ()
    in
    return @@ e_cond ~loc test ifso ifnot
  | I_Case { region ; value = {expr ; cases } } ->
    let loc = Location.lift region in
    let matchee = compile_expression ~raise expr in
    let cases = compile_matching_expr ~raise compile_if_clause (npseq_to_ne_list cases.value) in
    return @@ e_matching ~loc matchee cases
  | I_Assign {region ; value = { lhs ; rhs }} ->
    let loc = Location.lift region in
    let (var,path) = compile_lhs lhs in
    let rhs = compile_expression ~raise rhs in
    return @@ e_assign_ez ~loc var path rhs
  | Loop (While wl) ->
    let (wl, loc) = r_split wl in
    let cond = compile_expression ~raise wl.cond in
    let body = compile_block ~raise wl.block in
    return @@ e_while ~loc cond body
  | Loop (For (ForInt fl)) ->
    let (fl, loc) = r_split fl in
    let (binder, binder_loc) = r_split fl.binder in
    let start = compile_expression ~raise fl.init in
    let bound = compile_expression ~raise fl.bound in
    let increment = Option.value ~default:(e_int_z Z.one) @@
      Option.map ~f:(compile_expression ~raise <@ snd) fl.step
    in
    let body  = compile_block ~raise fl.block in
    return @@ e_for ~loc (Location.wrap ~loc:binder_loc @@ Var.of_name binder) start bound increment body
  | Loop (For (ForCollect el)) ->
    let (el, loc) = r_split el in
    let binder =
      let (key, loc) = r_split el.var in
      let key' = Location.wrap ~loc @@ Var.of_name key in
      let value = Option.map
        ~f:(fun x ->
          let (v,loc) = r_split (snd x) in
          Location.wrap ~loc @@ Var.of_name v)
        el.bind_to in
      (key',value)
    in
    let collection = compile_expression ~raise el.expr in
    let (collection_type, _) = match el.collection with
      Map loc -> (Map, loc) | Set loc -> (Set, loc) | List loc -> (List, loc)
    in
    let body = compile_block ~raise el.block in
    return @@ e_for_each ~loc binder collection collection_type body
  | ProcCall {value=(EVar var,args);region} ->
    let loc = Location.lift region in
    let (var, loc_var) = r_split var in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:(compile_expression ~raise) @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      let func = e_variable_ez ~loc:loc_var var in
      let args = compile_tuple_expression args in
      return @@ e_application ~loc func args
    )
  (*TODO: move to proper module*)
  | ProcCall {value=(EModA {value={module_name;field};region=_},args);region} when
    List.mem ~equal:Caml.(=) build_ins module_name.value ->
    let loc = Location.lift region in
    let fun_name = match field with
      EVar v -> v.value
      | EModA _ -> raise.raise @@ unknown_constant module_name.value loc
      |ECase _|ECond _|EAnnot _|EList _|EConstr _|EUpdate _|EFun _|ECodeInj _
      |ELogic _|EArith _|EString _|ERecord _|EProj _|ECall _|EBytes _|ETuple _|EPar _
      |ESet _|EMap _|EBlock _ -> failwith "Corner case : This couldn't be produce by the parser"
    in
    let var = module_name.value ^ "." ^ fun_name in
    (match constants var with
      Some const ->
      let (args, _) = r_split args in
      let args = List.map ~f:(compile_expression ~raise) @@ npseq_to_list args.inside in
      return @@ e_constant ~loc const args
    | None ->
      raise.raise @@ unknown_constant var loc
      )
  | ProcCall pc ->
    let (pc, loc) = r_split pc in
    let (func, args) = pc in
    let func = compile_expression ~raise func in
    let args = compile_tuple_expression args in
    return @@ e_application ~loc func args
  | Skip s ->
    let loc = Location.lift s in
    return @@ e_skip ~loc ()
  | RecordPatch rp ->
    let (rp, loc) = r_split rp in
    let (record, var, path) = compile_path rp.path in
    let (updates, _) = r_split rp.record_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux record (update: CST.field_assignment CST.reg) =
      let (update,loc) = r_split update in
      let path = [Access_record update.field_name.value] in
      let expr = compile_expression ~raise update.field_expr in
      e_update ~loc record path expr
    in
    let new_record = List.fold ~f:aux ~init:record updates in
    return @@ e_assign_ez ~loc var path @@ new_record
  | MapPatch mp ->
    let (mp, loc) = r_split mp in
    let (map, var, path) = compile_path mp.path in
    let (updates, _) = r_split mp.map_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux map (update: CST.binding CST.reg) =
      let (update,loc) = r_split update in
      let key = compile_expression ~raise update.source in
      let value = compile_expression ~raise update.image in
      e_map_add ~loc key value map
    in
    let new_map = List.fold ~f:aux ~init:map updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | SetPatch sp ->
    let (sp, loc) = r_split sp in
    let (set, var, path) = compile_path sp.path in
    let (updates, _) = r_split sp.set_inj in
    let updates = npseq_to_list updates.ne_elements in
    let aux set (update: CST.expr) =
      let key = compile_expression ~raise update in
      e_constant ~loc (Const C_SET_ADD) [key; set]
    in
    let new_map = List.fold ~f:aux ~init:set updates in
    return @@ e_assign_ez ~loc var path @@ new_map
  | MapRemove mr ->
    let (mr, loc) = r_split mr in
    let (map, var, path) = compile_path mr.map in
    let key = compile_expression ~raise mr.key in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc (Const C_MAP_REMOVE) [key;map]
  | SetRemove sr ->
    let (sr, loc) = r_split sr in
    let (set, var, path)  = compile_path sr.set in
    let ele  = compile_expression ~raise sr.element in
    return @@ e_assign_ez ~loc var path @@
      e_constant ~loc (Const C_SET_REMOVE) [ele;set]

and compile_let_destructuring ~raise :
  ?const:bool -> Location.t -> CST.expr -> CST.pattern -> AST.expression -> AST.type_expression option -> AST.expression =
    fun ?(const = false) loc value pattern body ty_opt ->
      let init = compile_expression ~raise value in
      let pattern = conv ~raise ~const pattern in
      let match_case = { pattern ; body } in
      let match_ = e_matching ~loc init [match_case] in
      match ty_opt with
      | Some t -> (e_annotation ~loc match_ t)
      | None -> match_

and compile_data_declaration ~raise : next:AST.expression -> CST.data_decl -> _ =
  fun ~next data_decl ->
  let return loc var ascr var_attr attr init =
    e_let_in ~loc {var;ascr;attributes=var_attr} attr init next
  in
  match data_decl with
    LocalConst const_decl -> (
      let cd, loc = r_split const_decl in
      let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) cd.const_type in
      match cd.pattern with
      | PVar name -> (
        let name, ploc = r_split name in
        let init = compile_expression ~raise cd.init in
        let p = Location.wrap ~loc:ploc @@ Var.of_name name.variable.value
        and attr = const_decl.value.attributes in
        let attr = compile_attributes attr in
        return loc p type_ Stage_common.Helpers.const_attribute attr init
      )
      | pattern ->
        (* not sure what to do with  attributes in that case *)
        compile_let_destructuring ~raise ~const:true loc cd.init pattern next type_
  )
  | LocalVar var_decl -> (
      let vd, loc = r_split var_decl in
      let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) vd.var_type in
      match vd.pattern with
      | PVar name ->
        let name, ploc = r_split name in
        let init = compile_expression ~raise vd.init in
        let p = Location.wrap ~loc:ploc @@ Var.of_name name.variable.value in
        return loc p type_ Stage_common.Helpers.var_attribute [] init
      | pattern ->
        (* not sure what to do with  attributes in that case *)
        compile_let_destructuring ~raise loc vd.init pattern next type_
  )
  | LocalFun fun_decl ->
      let fun_decl, loc = r_split fun_decl in
      let _fun_name, fun_var, fun_type, attr, lambda =
        compile_fun_decl ~raise fun_decl in
      return loc fun_var fun_type Stage_common.Helpers.empty_attribute attr lambda

  | LocalType type_decl ->
    let td,loc = r_split type_decl in
    let name,_ = r_split td.name in
    let rhs = compile_type_expression ~raise td.type_expr in
    let name = Var.of_name name in
    e_type_in ~loc name rhs next

  | LocalModule module_decl ->
    let md,loc = r_split module_decl in
    let name,_ = r_split md.name in
    let rhs = compile_module ~raise md.module_ in
    e_mod_in ~loc name rhs next

  | LocalModuleAlias module_alias ->
    let ma,loc = r_split module_alias in
    let alias,_ = r_split ma.alias in
    let binders,_ = List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list ma.binders in
    e_mod_alias ~loc alias binders next

and compile_statement ~raise : ?next:AST.expression -> CST.statement -> _ =
  fun ?next statement ->
  let return a = a in
  match statement with
    Instr i ->
      let i = compile_instruction ~raise ?next i in
      return (Some i)
  | Data dd ->
    let next = Option.value ~default:(e_skip ()) next in
    let dd = compile_data_declaration ~raise ~next dd
    in return (Some dd)

and compile_block ~raise : ?next:AST.expression -> CST.block CST.reg -> _ =
  fun ?next block ->
  let return a = a in
  let (block', _loc) = r_split block in
  let statements = npseq_to_list block'.statements in
  let aux statement next =
    let statement = compile_statement ~raise ?next statement
    in return statement
  in
  let block' = List.fold_right ~f:aux ~init:next statements in
  match block' with
    Some block -> return block
  | None -> raise.raise @@ block_start_with_attribute block

and compile_fun_decl ~raise : CST.fun_decl -> string * expression_variable * type_expression option * AST.attributes * expression =
  fun ({kwd_recursive; fun_name; param; ret_type; return=r; attributes}: CST.fun_decl) ->
  let return a = a in
  let (fun_name, loc) = r_split fun_name in
  let fun_binder = Location.wrap ~loc @@ Var.of_name fun_name in
  let ret_type =
    Option.map ~f:(compile_type_expression ~raise <@ snd) ret_type in
  let param = compile_parameters ~raise param in
  let result = compile_expression ~raise r in

  (* This handles the parameter case: *)
  let (lambda, fun_type) =
    match param with
      binder::[] ->
        let lambda : _ AST.lambda = {
          binder;
          output_type = ret_type;
          result }
        in lambda, Option.map ~f:(fun (a,b) -> t_function a b)
                   @@ Option.bind_pair (binder.ascr,ret_type)
    | lst ->
        let lst = Option.all @@ List.map ~f:(fun e -> e.ascr) lst in
        let input_type = Option.map ~f:t_tuple lst in
        let binder = Location.wrap @@ Var.fresh ~name:"parameters" () in
        let lambda : _ AST.lambda = {
          binder={var=binder;ascr=input_type;attributes=Stage_common.Helpers.empty_attribute};
          output_type = ret_type;
          result= e_matching_tuple (e_variable binder) param result;
          } in
        lambda, Option.map ~f:(fun (a,b) -> t_function a b)
                @@ Option.bind_pair (input_type,ret_type) in
  (* This handles the recursion *)
  let func = match kwd_recursive with
    Some reg ->
      let fun_type =
        trace_option ~raise (untyped_recursive_fun loc) @@ fun_type in
      return @@ e_recursive ~loc:(Location.lift reg) fun_binder fun_type lambda
  | None   ->
      return @@ make_e ~loc @@ E_lambda lambda
  in
  let attr = compile_attributes attributes in
  return (fun_name, fun_binder, fun_type, attr, func)

and compile_declaration ~raise : CST.declaration -> _ =
  fun decl ->
  let return reg decl =
    [Location.wrap ~loc:(Location.lift reg) decl] in
  match decl with
    TypeDecl {value={name; type_expr; params}; region} ->
    let name, _ = r_split name in
    let type_expr =
      let rhs = compile_type_expression ~raise type_expr in
      match params with
      | None -> rhs
      | Some x ->
        let lst = Utils.nsepseq_to_list x.value.inside in
        let aux : CST.type_var -> AST.type_expression -> AST.type_expression =
          fun param type_ ->
            let (param,ploc) = r_split param in
            let ty_binder = Location.wrap ~loc:ploc @@ Var.of_name param in
            t_abstraction ~loc:(Location.lift region) ty_binder () type_
        in
        List.fold_right ~f:aux ~init:rhs lst
    in
    return region @@ AST.Declaration_type {type_binder=Var.of_name name; type_expr; type_attr=[]}
  | ConstDecl {value={pattern; const_type; init; attributes; _}; region} -> (
    let attr = compile_attributes attributes in
    match pattern with
    | PVar name ->
      let name, loc = r_split name in
      let var = Location.wrap ~loc @@ Var.of_name name.variable.value in
      let ascr =
        Option.map ~f:(compile_type_expression ~raise <@ snd) const_type in
      let expr = compile_expression ~raise init in
      let binder = {var;ascr;attributes=Stage_common.Helpers.const_attribute} in
      return region @@ AST.Declaration_constant {name = Some name.variable.value; binder;attr;expr}
    | _ ->
      raise.raise (unsupported_top_level_destructuring region)
  )
  | FunDecl {value;region} ->
    let (name,var,ascr,attr,expr) = compile_fun_decl ~raise value in
    let binder = {var;ascr;attributes=Stage_common.Helpers.empty_attribute} in
    let ast = AST.Declaration_constant {name = Some name; binder;attr;expr}
    in return region ast

  | ModuleDecl {value={name; module_; _};region} ->
      let (name,_) = r_split name in
      let module_ = compile_module ~raise module_ in
      let ast = AST.Declaration_module  {module_binder=name; module_; module_attr=[]}
      in return region ast

  | ModuleAlias {value={alias; binders; _};region} ->
     let alias, _ = r_split alias in
     let binders, _ =
       List.Ne.unzip @@ List.Ne.map r_split @@ npseq_to_ne_list binders in
     let ast = AST.Module_alias {alias; binders}
     in return region ast

  | Directive _ -> []

and compile_module ~raise : CST.ast -> AST.module_ =
  fun t ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list t.decl
    in List.concat lst
