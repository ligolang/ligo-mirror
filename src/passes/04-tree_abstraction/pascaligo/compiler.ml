open Errors
open Trace
open Function

module CST = Cst.Pascaligo
module AST = Ast_imperative
module Attr = Lexing_shared.Attr

open AST

let nseq_to_list (hd, tl) = hd :: tl
let npseq_to_list (hd, tl) = hd :: (List.map ~f:snd tl)
let npseq_to_ne_list (hd, tl) = (hd, List.map ~f:snd tl)

open Predefined.Tree_abstraction.Pascaligo

let r_split = Location.r_split

let w_split (x: 'a CST.Wrap.t) : 'a * Location.t =
  (x#payload, Location.lift x#region)

let mk_var var =
  if String.compare var Var.wildcard = 0 then
    Var.fresh ()
  else Var.of_name var

let rec e_unpar : CST.expr -> CST.expr =
  function
    E_Par e -> e_unpar e.value.inside
  | e -> e

let compile_attributes : CST.attributes -> AST.attributes =
  fun attributes ->
    let attrs = List.map ~f:(fst <@ r_split) attributes
    and f = function
      _, Some (Attr.String value) -> Some value
    | _, None -> None
    in List.filter_map attrs ~f

let compile_selection : CST.selection -> 'a access * location = function
    FieldName name ->
      let name, loc = w_split name
      in Access_record name, loc
  | Component comp ->
      let (_, index), loc = w_split comp
      in Access_tuple index, loc

let rec compile_type_expression ~raise : CST.type_expr -> AST.type_expression =
  fun te ->
    let self = compile_type_expression ~raise in
    match te with
    | T_Var v ->
      let (v,loc) = w_split v in
      t_variable_ez ~loc v

    | T_Cart {region; value = (fst, _, rest)} ->
      let loc = Location.lift region in
      let lst = List.map ~f:self (fst::npseq_to_list rest) in
      t_tuple ~loc lst

    | T_Sum { region ; value } ->
      let loc = Location.lift region in
      let attr = compile_attributes value.attributes in
      let cases = Utils.nsepseq_to_list value.variants in
      let f : CST.variant CST.reg -> string * AST.type_expression * string list =
        fun { value = {ctor ; args ; attributes} ; region } ->
          let _loc = Location.lift region in
          let t = Option.value ~default:(t_unit ()) (Option.map ~f:(self <@ snd) args) in
          let case_attr = compile_attributes attributes in
          (ctor#payload, t, case_attr)
      in
      t_sum_ez_attr ~loc ~attr (List.map ~f cases)

    | T_App { region ; value = (type_constant, args) } -> (
      let loc = Location.lift region in
      let get_t_string_singleton_opt =
        function
        | CST.T_String s -> Some s#payload
        | _ -> None
      in
      let get_t_int_singleton_opt = function
        | CST.T_Int x ->
          let (_,z) = x#payload in
          Some z
        | _ -> None
      in
      match type_constant with
      | T_Var v when String.equal v#payload "michelson_or" -> (
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
      | T_Var v when String.equal v#payload "michelson_pair" -> (
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
      | T_Var v when String.equal v#payload "sapling_state" -> (
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
      | T_Var v when String.equal v#payload "sapling_transaction" -> (
        let lst = npseq_to_list args.value.inside in
        match lst with
        | [(a : CST.type_expr)] -> (
          let sloc = Location.lift @@ CST.type_expr_to_region a in
          let a' = trace_option ~raise (michelson_type_wrong te v) @@ get_t_int_singleton_opt a in
          let singleton = t_singleton ~loc:sloc (Literal_int a') in
          t_sapling_transaction ~loc singleton
          )
        | _ -> raise.raise @@ michelson_type_wrong_arity loc v
      )
      | T_Var type_constant -> (
        let operator = Var.of_name type_constant#payload in
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
          let (module_name, access_loc) = w_split x in
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
          | None -> (* type punning: { x } -> { x : x } *)
            let (v , loc) = w_split field_name in
            t_variable_ez ~loc v
          | Some (_colon , x) -> self x
        in
        let attributes = compile_attributes attributes in
        (field_name#payload , t ,attributes)
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
      let (var,loc) = w_split var in
      let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
      let b =
        let var = Location.wrap ~loc @@ match var with
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
        | P_Ctor x -> Some x#payload
        | _ -> None
      in
      match get_ctor constr with
      | Some "Unit" -> Location.wrap ~loc @@ P_unit
      | Some label ->
        let carg = match p_opt with
          | Some p -> self (CST.P_Tuple p)
          | None -> Location.wrap ~loc P_unit
        in
        Location.wrap ~loc @@ P_variant (Label label, carg)
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
    | P_Record { region ; value = { elements }} -> (
      let loc = Location.lift region in
      let lst = Utils.sepseq_to_list elements in
      let aux : CST.field_pattern CST.reg -> AST.label * AST.ty_expr AST.pattern =
        fun x ->
          let (field, _) = r_split x in
          match field with
          | Punned { pun ; attributes } ->
            let () = List.iter ~f:check_no_attributes [attributes] in
            let (pun,loc) = w_split pun in
            let attributes = if const then Stage_common.Helpers.const_attribute else Stage_common.Helpers.var_attribute in
            let binder = { var = Location.wrap ~loc (Var.of_name pun) ; ascr = None ; attributes } in
            (AST.Label pun , Location.wrap ~loc (P_var binder))
          | Complete { field_lhs; field_rhs ; attributes ; _} ->
            let () = check_no_attributes attributes in
            (AST.Label field_lhs#payload, self field_rhs)
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
    | P_Ctor x -> (
      let (c,loc) = w_split x in
      match x#payload with
      | "Unit" -> Location.wrap ~loc P_unit
      | _ -> Location.wrap ~loc (P_variant (Label c, Location.wrap P_unit))
    )
    | P_ModPath _ | P_Mutez _ | P_Bytes _ | P_Int _ | P_Nat _ | P_String _ -> raise.raise @@ unsupported_pattern_type p

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
      let (var, loc) = w_split param.var in
      let var = Location.wrap ~loc @@ Var.of_name var in
      let ascr = Option.map ~f:(compile_type_expression ~raise <@ snd) param.param_type in
      { var ; ascr ; attributes = Stage_common.Helpers.const_attribute }
    | `Const _ ->
      let (var, loc) = w_split param.var in
      let var = Location.wrap ~loc @@ Var.of_name var in
      let ascr = Option.map ~f:(compile_type_expression ~raise  <@ snd) param.param_type in
      { var ; ascr ; attributes = Stage_common.Helpers.var_attribute }
  in
  let (params, _loc) = r_split params in
  let params = npseq_to_list params.inside in
  List.map ~f:aux params

and compile_instruction ~raise : ?next: AST.expression -> CST.instruction -> AST.expression  = fun ?next instruction ->
  let return expr = Option.value_map next ~default:expr ~f:(e_sequence expr) in
  let rec get_var : CST.expr -> AST.expression_variable option =
    function
    | E_Par x -> get_var x.value.inside
    | E_Var v -> let (v,loc) = w_split v in
                Some (Location.wrap ~loc (Var.of_name v))
    | _ -> None
  in
  let compile_path : (CST.selection, CST.dot) Utils.nsepseq -> AST.expression AST.access list =
    fun x ->
      let f : CST.selection -> AST.expression AST.access = function
        | FieldName name -> Access_record name#payload
        | Component v -> Access_tuple (snd v#payload)
      in
      List.map (Utils.nsepseq_to_list x) ~f
  in
  let rec compile_lvalue : CST.expr -> AST.expression AST.access list -> AST.expression_variable * AST.expression AST.access list =
    (*
      compile_lvalue [lhs] []
      compile the lvalue [lhs] (left side on an assignment) in the form of a variable and an access list, e.g:
        - ((r.x).y).z          |-> (r, [x;y;z])
        - (m.["foo"]).["bar"]  |-> (m, ["foo";"bar"])

      Note: if the left part of [lhs] isn't a variable, this function will reject it, e.g:
        - { x = 1 ; y = 2}.x = 2
    *)
    fun lhs cpath ->
      match lhs with
      | E_Par x -> compile_lvalue x.value.inside cpath
      | E_Var v -> (
        let (v,loc) = w_split v in
        let v = Location.wrap ~loc (Var.of_name v) in
        (v, cpath)
      )
      | E_Proj { value = { record_or_tuple ; field_path } } -> (
        let path = cpath @ compile_path field_path in
        match get_var record_or_tuple with
        | Some v -> (v, path)
        | None -> compile_lvalue record_or_tuple path
      )
      | E_MapLookup { value = { map ; index } } -> (
        let index = compile_expression ~raise index.value.inside in
        let path = cpath @ [Access_map index] in
        match get_var map with
        | Some v -> (v,path)
        | None -> compile_lvalue map path
      )
      | E_ModPath _ -> failwith "TODO: error, unsuported assignements for modules at region"
      | _ -> failwith "TODO: error, invalid lvalue at region"
  in
  let compile_if_clause : ?next:AST.expression -> CST.test_clause -> AST.expression =
    fun ?next if_clause ->
      match if_clause with
      | ClauseInstr i -> compile_instruction ~raise ?next i
      | ClauseBlock block -> compile_block ~raise ?next block
  in
  match instruction with
  | I_Cond { region ; value = { test ; if_so ; if_not } } -> (
    let loc = Location.lift region in
    let test = compile_expression ~raise test in
    let ifso = compile_if_clause if_so in
    let ifnot = Option.value_map if_not ~default:(e_skip ()) ~f:(fun x -> compile_if_clause (snd x)) in
    return @@ e_cond ~loc test ifso ifnot
  )
  | I_Case { region ; value = {expr ; cases } } -> (
    let loc = Location.lift region in
    let matchee = compile_expression ~raise expr in
    let cases = compile_matching_expr ~raise compile_if_clause (npseq_to_ne_list cases) in
    return @@ e_matching ~loc matchee cases
  )
  | I_Assign {region ; value = { lhs ; rhs }} -> (
    let loc = Location.lift region in
    let rhs = compile_expression ~raise rhs in
    let (v,path) = compile_lvalue lhs [] in
    return @@ e_assign ~loc v path rhs
  )
  | I_While { region ; value = { cond ; block } } -> (
    let loc = Location.lift region in
    let cond = compile_expression ~raise cond in
    let body = compile_block ~raise block in
    return @@ e_while ~loc cond body
  )
  | I_For { value = { index ; init ; bound ; step ; block} ; region } -> (
    let loc = Location.lift region in
    let index =
      let (index,loc) = w_split index in
      Location.wrap ~loc (Var.of_name index)
    in
    let start = compile_expression ~raise init in
    let bound = compile_expression ~raise bound in
    let increment = Option.value_map step ~default:(e_int_z Z.one) ~f:(compile_expression ~raise <@ snd) in
    let body  = compile_block ~raise block in
    return @@ e_for ~loc index start bound increment body
  )
  | I_ForIn { region ; value = { var ; bind_to ; expr ; block } } -> (
    let loc = Location.lift region in
    let binder =
      let (key, loc) = w_split var in
      let key' = Location.wrap ~loc @@ Var.of_name key in
      let value = Option.map bind_to
        ~f:(fun x ->
              let (v,loc) = w_split (snd x) in
              Location.wrap ~loc @@ Var.of_name v
           )
      in
      (key',value)
    in
    let collection = compile_expression ~raise expr in
    let body = compile_block ~raise block in
    return @@ e_for_each ~loc binder collection body
  )
  | I_Skip s -> (
    let loc = Location.lift s#region in
    return @@ e_skip ~loc ()
  )
  | I_Patch { region ; value = { collection ; patch } } -> (
    let loc = Location.lift region in
    let rhs =
      let init = compile_expression ~raise collection in
      match e_unpar patch with
      | E_Record { region = _ ; value = { elements ; attributes = _ } } -> (
        (*
          patch <collection> with <patch>
          e.g.
           `patch c with record [ x.u = 2 ; y ]` |-> `c := update (update (c, [x;u], 2) , [y], y)`
        *)
        let lst = Utils.sepseq_to_list elements in
        let f : AST.expression -> (CST.expr , CST.expr) CST.field CST.reg -> AST.expression =
          fun cur field ->
            let loc = Location.lift field.region in
            match field.value with
            | Punned { pun ; attributes=_ } -> (
              match get_var pun with
              | Some v -> e_update ~loc cur [Access_record (Var.to_name v.wrap_content)] (e_variable v)
              | None ->  failwith "TODO: Error, wrong update field at _loc"
            )
            | Complete { field_lhs ; field_rhs } -> (
              let rhs = compile_expression ~raise field_rhs in
              match field_lhs with
              | E_Proj { region = _ ; value = { record_or_tuple ; field_path } } -> (
                let v = match get_var record_or_tuple with
                  | Some x -> Var.to_name x.wrap_content
                  | None ->
                    let _loc = CST.expr_to_region record_or_tuple in
                    failwith "TODO: error, 'patch collection with record [ {x=2;y=1}.x = 2 ; .. ]' at _loc"
                in
                let path = (Access_record v) :: (compile_path field_path) in
                e_update ~loc cur path rhs
              )
              | _ -> let _loc = Location.lift field.region in failwith "TODO: error, wrong update field at _loc"
            )
        in
        List.fold lst ~f ~init
      )
      | E_Map { region = _ ; value = { elements ; attributes = _ } } -> (
        let lst = Utils.sepseq_to_list elements in
        let f : AST.expression -> CST.binding CST.reg -> AST.expression =
          fun cur { region ; value = { key ; value } } ->
            let loc = Location.lift region in
            let source = compile_expression ~raise key in
            let image = compile_expression ~raise value in
            let path = [ Access_map source] in
            e_update ~loc cur path image
        in
        List.fold lst ~f ~init
      )
      | E_Set { region = _ ; value = { elements ; attributes = _ } } -> (
        let lst = Utils.sepseq_to_list elements in
        let f : AST.expression -> CST.expr -> AST.expression =
          fun cur expr ->
            let loc = Location.lift (CST.expr_to_region expr) in
            let el = compile_expression ~raise expr in
            (* TODO: should we emit an E_update as for maps/records (need Access_set) *)
            e_constant ~loc (Const C_SET_ADD) [el;cur]
        in
        List.fold lst ~f ~init
      )
      | _ -> failwith "TODO: error, wrong patch rhs"
    in
    let (v,path) = compile_lvalue collection [] in
    return @@ e_assign ~loc v path rhs
  )
  | I_Remove { region ; value = { item; collection }} -> (
    let loc = Location.lift region in
    let (v,path) = compile_lvalue collection [] in
    let collection = compile_expression ~raise collection in
    let item = compile_expression ~raise item in
    return @@ e_assign ~loc v path (e_constant (Const C_MAP_REMOVE) [item;collection])
  )
  | I_Call { region ; value = (f,args) } -> (
    return @@ compile_expression ~raise (E_App { region ; value = (f,Some args)})
  )

and compile_let_destructuring ~raise :
  ?const:bool -> Location.t -> CST.expr -> CST.pattern -> AST.expression -> AST.type_expression option -> AST.expression =
    fun ?(const = false) loc value pattern body ty_opt ->
      let init = compile_expression ~raise value in
      let pattern = conv ~raise ~const pattern in
      let match_case = { pattern ; body } in
      let match_ = e_matching ~loc init [match_case] in
      Option.value_map ty_opt ~default:match_ ~f:(e_annotation ~loc match_)

and compile_data_declaration ~raise : next:AST.expression -> CST.declaration -> AST.expression =
  fun ~next data_decl ->
  let return loc var ascr var_attr attr init =
    e_let_in ~loc {var;ascr;attributes=var_attr} attr init next
  in
  match data_decl with
  | D_Const const_decl -> (
    let cd, loc = r_split const_decl in
    let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) cd.const_type in
    match cd.pattern with
    | P_Var name -> (
      let name, ploc = w_split name in
      let init = compile_expression ~raise cd.init in
      let p = Location.wrap ~loc:ploc @@ Var.of_name name
      and attr = const_decl.value.attributes in
      let attr = compile_attributes attr in
      return loc p type_ Stage_common.Helpers.const_attribute attr init
    )
    | pattern ->
      (* not sure what to do with  attributes in that case *)
      compile_let_destructuring ~raise ~const:true loc cd.init pattern next type_
  )
  | D_Directive _ -> next
  | D_Fun fun_decl -> (
    let fun_decl, loc = r_split fun_decl in
    let _fun_name, fun_var, fun_type, attr, lambda = compile_fun_decl ~raise fun_decl in
    return loc fun_var fun_type Stage_common.Helpers.empty_attribute attr lambda
  )
  | D_Type type_decl -> (
    let td,loc = r_split type_decl in
    let name,_ = w_split td.name in
    let rhs = compile_type_expression ~raise td.type_expr in
    let name = Var.of_name name in
    e_type_in ~loc name rhs next
  )
  | D_Module module_decl -> (
    let md,loc = r_split module_decl in
    let name,_ = w_split md.name in
    let rhs = compile_module ~raise md.declarations in
    e_mod_in ~loc name rhs next
  )
  | D_ModAlias module_alias -> (
    let ma,loc = r_split module_alias in
    let alias,_ = w_split ma.alias in
    let lst = Utils.nsepseq_to_list ma.mod_path in
    let binders = List.map lst ~f:(fun x -> fst (w_split x)) in
    e_mod_alias ~loc alias (List.Ne.of_list binders) next
  )

and compile_statement ~raise : ?next:AST.expression -> CST.statement -> AST.expression option =
  fun ?next statement ->
  match statement with
  | S_Instr i ->
    let i = compile_instruction ~raise ?next i in
    Some i
  | S_Decl dd ->
    let next = Option.value ~default:(e_skip ()) next in
    let dd = compile_data_declaration ~raise ~next dd in
    (Some dd)
  | S_VarDecl var_decl -> (
    let vd, loc = r_split var_decl in
    let type_ = Option.map ~f:(compile_type_expression ~raise <@ snd) vd.var_type in
    match vd.pattern with
    | P_Var name -> (
      let name, ploc = w_split name in
      let init = compile_expression ~raise vd.init in
      let var = Location.wrap ~loc:ploc @@ Var.of_name name in
      match next with
      | Some next ->
        Some (e_let_in ~loc {var;ascr=type_;attributes=Stage_common.Helpers.var_attribute} [] init next)
      | None -> None
    )
    | pattern ->
      (* not sure what to do with  attributes in that case *)
      let next = trace_option ~raise (failwith "TODO: error, ...") next in
      let x = compile_let_destructuring ~raise ~const:false loc vd.init pattern next type_ in
      Some x
  )

and compile_block ~raise : ?next:AST.expression -> CST.block CST.reg -> AST.expression =
  fun ?next block ->
    let (block', _loc) = r_split block in
    let statements = npseq_to_list block'.statements in
    let aux statement next = compile_statement ~raise ?next statement in
    let block' = List.fold_right ~f:aux ~init:next statements in
    match block' with
    | Some block -> block
    | None -> raise.raise @@ block_start_with_attribute block

and compile_fun_decl ~raise : CST.fun_decl -> string * expression_variable * type_expression option * AST.attributes * expression =
  fun ({kwd_recursive; fun_name; param; ret_type; return=r; attributes}: CST.fun_decl) ->
  let return a = a in
  let (fun_name, loc) = w_split fun_name in
  let fun_binder = Location.wrap ~loc @@ Var.of_name fun_name in
  let ret_type = Option.map ~f:(compile_type_expression ~raise <@ snd) ret_type in
  let param = compile_parameters ~raise param in
  let result = compile_expression ~raise r in
  let (lambda, fun_type) =
    match param with
    | binder::[] ->
      let lambda : _ AST.lambda = { binder; output_type = ret_type; result } in
      (lambda , Option.map ~f:(fun (a,b) -> t_function a b) @@ Option.bind_pair (binder.ascr,ret_type))
    | lst ->
      let lst = Option.all @@ List.map ~f:(fun e -> e.ascr) lst in
      let input_type = Option.map ~f:t_tuple lst in
      let var = Location.wrap @@ Var.fresh ~name:"parameters" () in
      let binder = { var;ascr=input_type;attributes=Stage_common.Helpers.empty_attribute} in
      let result = e_matching_tuple (e_variable var) param result in
      let lambda : _ AST.lambda = { binder ; output_type = ret_type ; result } in
      (lambda, Option.map ~f:(fun (a,b) -> t_function a b) @@ Option.bind_pair (input_type,ret_type))
  in
  let func =
    match kwd_recursive with
    | Some reg ->
      let fun_type = trace_option ~raise (untyped_recursive_fun loc) @@ fun_type in
      return @@ e_recursive ~loc:(Location.lift reg#region) fun_binder fun_type lambda
    | None -> return @@ make_e ~loc @@ E_lambda lambda
  in
  let attr = compile_attributes attributes in
  return (fun_name, fun_binder, fun_type, attr, func)

and compile_declaration ~raise : CST.declaration -> (expression, type_expression) declaration' location_wrap list =
  fun decl ->
  let return reg decl = [Location.wrap ~loc:(Location.lift reg) decl] in
  match decl with
  | D_Type {value={name; type_expr; params}; region} ->
      let name, _ = w_split name in
      let type_expr =
        let rhs = compile_type_expression ~raise type_expr in
        match params with
        | None -> rhs
        | Some x ->
            let lst = Utils.nsepseq_to_list x.value.inside in
            let aux : CST.variable -> AST.type_expression -> AST.type_expression =
              fun param type_ ->
                let (param,ploc) = w_split param in
                let ty_binder = Location.wrap ~loc:ploc @@ Var.of_name param in
                t_abstraction ~loc:(Location.lift region) ty_binder () type_
            in List.fold_right ~f:aux ~init:rhs lst in
      let ast =
        AST.Declaration_type {type_binder=Var.of_name name;
                              type_expr; type_attr=[]}
      in return region ast

  | D_Const {value={pattern; const_type; init; attributes; _}; region} -> (
      let attr = compile_attributes attributes in
      match pattern with
      | P_Var name ->
          let name, loc = w_split name in
          let var = Location.wrap ~loc @@ Var.of_name name in
          let ascr =
            Option.map ~f:(compile_type_expression ~raise <@ snd) const_type in
          let expr = compile_expression ~raise init in
          let attributes = Stage_common.Helpers.const_attribute in
          let binder = {var; ascr; attributes} in
          let ast =
            AST.Declaration_constant {name = Some name; binder; attr; expr}
          in return region ast
      | _ ->
         raise.raise (unsupported_top_level_destructuring region)
    )

  | D_Fun {value; region} ->
      let name, var, ascr, attr, expr = compile_fun_decl ~raise value in
      let attributes = Stage_common.Helpers.empty_attribute in
      let binder = {var; ascr; attributes} in
      let ast = AST.Declaration_constant {name = Some name; binder; attr; expr}
      in return region ast

  | D_Module {value; region} ->
      let {name; declarations } : CST.module_decl = value in
      let name, _ = w_split name in
      let module_ = compile_module ~raise declarations in
      let ast =
        AST.Declaration_module {module_binder=name; module_; module_attr=[]}
      in return region ast

  | D_ModAlias {value; region} ->
      let {alias; mod_path; _} : CST.module_alias = value in
      let alias, _ = w_split alias in
      let lst = Utils.nsepseq_to_list mod_path in
      let binders =
        List.Ne.of_list @@ List.map lst ~f:(fun x -> fst (w_split x)) in
      let ast = AST.Module_alias {alias; binders}
      in return region ast

  | D_Directive _ -> []

and compile_module ~raise : CST.declaration Utils.nseq -> AST.module_ =
  fun decl ->
    let lst = List.map ~f:(compile_declaration ~raise) @@ nseq_to_list decl
    in List.concat lst
