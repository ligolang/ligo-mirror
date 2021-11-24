module AST = Ast_imperative
module CST = Cst.Cameligo
module Predefined = Predefined.Tree_abstraction.Cameligo

module Wrap = Lexing_shared.Wrap

open Function

(* Utils *)

let decompile_attributes = List.map ~f:Wrap.wrap

let list_to_sepseq lst =
  match lst with
    [] -> None
  |  hd :: lst ->
      let aux e = Wrap.ghost "", e in
      Some (hd, List.map ~f:aux lst)

let list_to_nsepseq lst =
  match list_to_sepseq lst with
    Some s -> s
  | None   -> failwith "List is empty"

let nelist_to_npseq (hd, lst) =
  (hd, List.map ~f:(fun e -> (Wrap.ghost "", e)) lst)

let npseq_cons hd lst = hd, (Wrap.ghost "", fst lst)::(snd lst)

let par a = CST.{lpar=Wrap.ghost ""; inside=a; rpar=Wrap.ghost ""}

let type_vars_of_list : string Region.reg list -> CST.type_vars =
  fun lst ->
  let type_var_of_name : _ -> CST.type_var Region.reg =
    fun name -> Region.wrap_ghost CST.{quote=Wrap.ghost ""; name} in
  match lst with
  | [name] -> QParam (type_var_of_name name)
  | x ->
    let x = Utils.nsepseq_map type_var_of_name (list_to_nsepseq x) in
    QParamTuple (Region.wrap_ghost (par x))

let inject compound a = CST.{compound;elements=a;terminator=None}

let ne_inject compound fields ~attr = CST.{
  compound;
  ne_elements=fields;
  terminator=None;
  attributes=attr
  }

let prefix_colon a = (Wrap.ghost "", a)

let braces   = Some (CST.Braces (Wrap.ghost "", Wrap.ghost ""))
let brackets = Some (CST.Brackets (Wrap.ghost "", Wrap.ghost ""))
let beginEnd = Some (CST.BeginEnd (Wrap.ghost "", Wrap.ghost ""))

(* Decompiler *)

let decompile_variable : type a. a Var.t -> CST.variable = fun var ->
  let var = Format.asprintf "%a" Var.pp var in
  if String.contains var '#' then
    let var = String.split_on_char '#' var in
    Region.wrap_ghost @@ "gen__" ^ String.concat "" var
  else
    if String.length var > 4
       && String.equal "gen__" @@ String.sub var 0 5 then
      Region.wrap_ghost @@ "user__" ^ var
    else
      Region.wrap_ghost var

let rec decompile_type_expr : AST.type_expression -> CST.type_expr = fun te ->
  let return te = te in
  match te.type_content with
    T_sum { attributes ; fields } ->
    let attributes = decompile_attributes attributes in
    let lst = AST.LMap.to_kv_list fields in
    let aux (AST.Label c, AST.{associated_type; attributes=row_attr; _}) =
      let constr = Region.wrap_ghost c in
      let arg = decompile_type_expr associated_type in
      let arg = Some (Wrap.ghost "", arg) in
      let row_attr = decompile_attributes row_attr in
      let variant : CST.variant = {constr; arg; attributes=row_attr} in
      Region.wrap_ghost variant in
    let variants = List.map ~f:aux lst in
    let variants = list_to_nsepseq variants in
    let lead_vbar = Some (Wrap.ghost "") in
    let sum : CST.sum_type = {lead_vbar; variants; attributes} in
    return @@ CST.TSum (Region.wrap_ghost sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes=field_attr; _}) =
      let field_name = Region.wrap_ghost c in
      let colon = Wrap.ghost "" in
      let field_type = decompile_type_expr associated_type in
      let field_attr = decompile_attributes field_attr in
      let field : CST.field_decl =
        {field_name; colon; field_type; attributes=field_attr} in
      Region.wrap_ghost field in
    let record = List.map ~f:aux record in
    let record = list_to_nsepseq record in
    let attributes = decompile_attributes attributes in
    return @@ CST.TRecord (Region.wrap_ghost
                          @@ ne_inject braces record ~attr:attributes)
  | T_tuple tuple ->
    let tuple = List.map ~f:decompile_type_expr tuple in
    let tuple = list_to_nsepseq @@ tuple in
    return @@ CST.TProd (Region.wrap_ghost tuple)
  | T_arrow {type1;type2} ->
    let type1 = decompile_type_expr type1 in
    let type2 = decompile_type_expr type2 in
    let arrow = (type1, Wrap.ghost "", type2) in
    return @@ CST.TFun (Region.wrap_ghost arrow)
  | T_variable variable ->
    let var = decompile_variable variable in
    return @@ CST.TVar var
  | T_app {type_operator; arguments} ->
    let type_constant = Region.wrap_ghost @@ Var.to_name type_operator in
    let arguments = List.map ~f:decompile_type_expr arguments in
    let arguments = list_to_nsepseq arguments in
    let par : _ CST.par =
      {lpar=Wrap.ghost ""; inside=arguments; rpar=Wrap.ghost ""} in
    let lst : CST.type_ctor_arg =
      CST.CArgTuple (Region.wrap_ghost par) in
    return @@ CST.TApp (Region.wrap_ghost (type_constant, lst))
  | T_annoted _annot ->
    failwith "let's work on it later"
  | T_module_accessor {module_name;element} ->
     (* TODO: module_path au lieu de module_name *)
    let module_name = Region.wrap_ghost module_name in
    let field  = decompile_type_expr element in
    return @@
      CST.TModPath (Region.wrap_ghost
                      CST.{module_path; selector=wrap_ghost;field})
  | T_singleton x -> (
    match x with
    | Literal_int i ->
      let z : CST.type_expr = CST.TInt { region = Region.ghost ; value = (Z.to_string i, i) } in
      return z
    | _ -> failwith "unsupported singleton"
  )
  | T_abstraction x -> decompile_type_expr x.type_
  | T_for_all x -> decompile_type_expr x.type_

let get_e_variable : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_variable var -> var.wrap_content
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let get_e_tuple : AST.expression -> _  = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> (tuple, false)
  | E_variable _
  | E_literal _
  | E_constant _
  | E_lambda _ -> ([expr], false)
  | E_application _ -> ([expr], true)
  | E_accessor _
  | E_module_accessor _ -> ([expr], false)
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr

let pattern_type ({var;ascr;attributes}: _ AST.binder) =
  let attributes = attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> decompile_attributes in
  let var : CST.var_pattern = {variable = decompile_variable var.wrap_content; attributes = attributes} in
  let pattern : CST.pattern = CST.PVar (Region.wrap_ghost var) in
  match ascr with
    Some s ->
      let type_expr = decompile_type_expr s in
      CST.PTyped (Region.wrap_ghost @@
                    CST.{pattern; colon=Wrap.ghost ""; type_expr})
  | None -> pattern

let decompile_type_params : AST.type_expression -> _ option * CST.type_expr = fun type_expr ->
  let q, t = AST.Combinators.destruct_for_alls type_expr in
  let t = decompile_type_expr t in
  match q with
  | [] -> None, t
  | _ -> let type_vars = List.rev @@ List.map q ~f:decompile_variable in
         let type_vars = List.Ne.of_list type_vars in
         let inside : CST.type_params =
           {kwd_type = Wrap.ghost ""; type_vars} in
         let q : _ CST.par =
           {lpar=Wrap.ghost ""; inside; rpar=Wrap.ghost ""} in
         let q = Region.wrap_ghost q in
         Some q, t

let rec decompile_expression : AST.expression -> CST.expr = fun expr ->
  let return_expr expr = expr in
  let return_expr_with_par expr =
    return_expr @@ CST.EPar (Region.wrap_ghost @@ par expr) in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.EVar var
  | E_constant {cons_name; arguments} ->
     let expr =
       CST.EVar (Region.wrap_ghost @@
                   Predefined.constant_to_string cons_name) in
    (match arguments with
      [] -> return_expr @@ expr
    | _ ->
       let arguments =
         List.Ne.of_list @@
           (List.map ~f:(fun x -> CST.EPar (Region.wrap_ghost @@ par x))) @@
             List.map ~f:decompile_expression arguments in
       let const = Region.wrap_ghost (expr, arguments) in
       return_expr_with_par @@ CST.ECall const
    )
  | E_literal literal ->
    (match literal with
       Literal_unit  ->
         return_expr @@
           CST.EUnit (Region.wrap_ghost (Wrap.ghost "", Wrap.ghost ""))
     | Literal_int i ->
         return_expr @@ CST.EArith (Int (Region.wrap_ghost ("",i)))
     | Literal_nat n ->
        return_expr @@ CST.EArith (Nat (Region.wrap_ghost ("",n)))
     | Literal_timestamp time ->
        let time = Tezos_utils.Time.Protocol.to_notation @@
          Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time in
          (* TODO combinators for CSTs. *)
        let ty = decompile_type_expr @@ AST.t_timestamp () in
        let time = CST.EString (String (Region.wrap_ghost time)) in
        return_expr @@
          CST.EAnnot (Region.wrap_ghost @@ par (time, Wrap.ghost "", ty))
     | Literal_mutez mtez ->
         return_expr @@ CST.EArith (Mutez (Region.wrap_ghost ("",mtez)))
     | Literal_string (Standard str) ->
         return_expr @@ CST.EString (String (Region.wrap_ghost str))
      | Literal_string (Verbatim ver) -> return_expr @@ CST.EString (Verbatim (Region.wrap_ghost ver))
      | Literal_bytes b ->
        let b = Hex.of_bytes b in
        let s = Hex.to_string b in
        return_expr @@ CST.EBytes (Region.wrap_ghost (s,b))
      | Literal_address addr ->
        let addr = CST.EString (String (Region.wrap_ghost addr)) in
        let ty = decompile_type_expr @@ AST.t_address () in
        return_expr @@
          CST.EAnnot (Region.wrap_ghost @@ par (addr, Wrap.ghost "",ty))
      | Literal_signature sign ->
        let sign = CST.EString (String (Region.wrap_ghost sign)) in
        let ty = decompile_type_expr @@ AST.t_signature () in
        return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (sign, Wrap.ghost "",ty))
      | Literal_key k ->
        let k = CST.EString (String (Region.wrap_ghost k)) in
        let ty = decompile_type_expr @@ AST.t_key () in
        return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (k,Wrap.ghost "",ty))
      | Literal_key_hash kh ->
        let kh = CST.EString (String (Region.wrap_ghost kh)) in
        let ty = decompile_type_expr @@ AST.t_key_hash () in
        return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (kh,Wrap.ghost "",ty))
      | Literal_chain_id _
      | Literal_operation _ ->
        failwith "chain_id, operation are not created currently ?"
    )
  | E_application {lamb;args} ->
    let f (expr, b) = if b then CST.EPar (Region.wrap_ghost @@ par expr) else expr in
    let lamb = decompile_expression lamb in
    let args = List.Ne.of_list @@
      List.map ~f @@
        (fun (e, b) -> List.map ~f:(fun e ->
                                let de = decompile_expression e in
                                (de, b)) e) @@
      get_e_tuple args
    in
    return_expr @@ CST.ECall (Region.wrap_ghost (lamb,args))
  | E_lambda lambda ->
    let (binders,_lhs_type,_block_with,body) = decompile_lambda lambda in
    let fun_expr : CST.fun_expr = {kwd_fun=Wrap.ghost "";binders;lhs_type=None;arrow=Wrap.ghost "";body;type_params=None;attributes=[]} in
    return_expr_with_par @@ CST.EFun (Region.wrap_ghost fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder={var;ascr;attributes=var_attributes};rhs;let_result;attributes} ->
    let var_attributes = var_attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> decompile_attributes in
    let var : CST.pattern = CST.PVar (Region.wrap_ghost ({variable = decompile_variable @@ var.wrap_content; attributes = var_attributes } : CST.var_pattern)) in
    let binders = (var,[]) in
    let type_params, lhs_type =
      Option.value_map ascr ~default:(None, None)
                       ~f:(fun t -> let type_params, lhs_type =
                                   decompile_type_params t in
                                 type_params, Some (prefix_colon lhs_type)) in
    let let_rhs = decompile_expression rhs in
    let binding : CST.let_binding = {binders;type_params;lhs_type;eq=Wrap.ghost "";let_rhs} in
    let body = decompile_expression let_result in
    let attributes = decompile_attributes attributes in
    let lin : CST.let_in = {kwd_let=Wrap.ghost "";kwd_rec=None;binding;kwd_in=Wrap.ghost "";body;attributes} in
    return_expr @@ CST.ELetIn (Region.wrap_ghost lin)
  | E_type_in {type_binder;rhs;let_result} ->
    let name = decompile_variable type_binder in
    let type_expr = decompile_type_expr rhs in
    let type_decl : CST.type_decl = {kwd_type=Wrap.ghost "";params=None;name;eq=Wrap.ghost "";type_expr} in
    let body = decompile_expression let_result in
    let tin : CST.type_in = {type_decl;kwd_in=Wrap.ghost "";body} in
    return_expr @@ CST.ETypeIn (Region.wrap_ghost tin)
  | E_mod_in {module_binder;rhs;let_result} ->
    let name = Region.wrap_ghost module_binder in
    let module_ = decompile_module rhs in
    let mod_decl : CST.module_decl = {kwd_module=Wrap.ghost "";name;eq=Wrap.ghost "";kwd_struct=Wrap.ghost "";module_;kwd_end=Wrap.ghost ""} in
    let body = decompile_expression let_result in
    let min : CST.mod_in = {mod_decl;kwd_in=Wrap.ghost "";body} in
    return_expr @@ CST.EModIn (Region.wrap_ghost min)
  | E_mod_alias {alias; binders; result} ->
    let alias   = Region.wrap_ghost alias in
    let binders = nelist_to_npseq @@ List.Ne.map Region.wrap_ghost binders in
    let mod_alias : CST.module_alias = {kwd_module=Wrap.ghost "";alias;eq=Wrap.ghost "";binders} in
    let body = decompile_expression result in
    let mod_alias : CST.mod_alias = {mod_alias;kwd_in=Wrap.ghost "";body} in
    return_expr @@ CST.EModAlias (Region.wrap_ghost mod_alias)
  | E_raw_code {language; code} ->
    let language = Region.wrap_ghost @@ Region.wrap_ghost language in
    let code = decompile_expression code in
    let ci : CST.code_inj = {language;code;rbracket=Wrap.ghost ""} in
    return_expr @@ CST.ECodeInj (Region.wrap_ghost ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = Region.wrap_ghost constr in
    let element = decompile_expression element in
    return_expr_with_par @@ CST.EConstr (Region.wrap_ghost (constr, Some element))
  | E_matching {matchee; cases} ->
    let expr  = decompile_expression matchee in
    let aux : _ AST.match_case -> _ CST.case_clause CST.reg =
      fun { pattern ; body } ->
        let rhs = decompile_expression body in
        let pattern = decompile_pattern pattern in
        (Region.wrap_ghost ({pattern ; arrow = Wrap.ghost "" ; rhs }:_ CST.case_clause))
    in
    let case_clauses = List.map ~f:aux cases in
    let cases = list_to_nsepseq case_clauses in
    let cases = Region.wrap_ghost cases in
    let cases : _ CST.case = {kwd_match=Wrap.ghost "";expr;kwd_with=Wrap.ghost "";lead_vbar=None;cases} in
    return_expr @@ CST.ECase (Region.wrap_ghost cases)
  | E_record record ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = Region.wrap_ghost str in
      let field_expr = decompile_expression expr in
      let field : CST.field_assign = {field_name;assignment=Wrap.ghost "";field_expr} in
      Region.wrap_ghost field
    in
    let record = List.map ~f:aux record in
    let record = list_to_nsepseq record in
    let record = ne_inject braces record ~attr:[] in
    (* why is the record not empty ? *)
    return_expr @@ CST.ERecord (Region.wrap_ghost record)
  | E_accessor {record; path} ->
    (match List.rev path with
      Access_map e :: [] ->
      let map = decompile_expression record in
      let e = decompile_expression e in
      let arg = e,[map] in
      return_expr @@ CST.ECall( Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.find_opt"), arg))
    | Access_map e :: lst ->
      let path = List.rev lst in
      let field_path = list_to_nsepseq @@ List.map ~f:decompile_to_selection path in
      let struct_name = decompile_variable @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=Wrap.ghost "";field_path} in
      let e = decompile_expression e in
      let arg = e,[CST.EProj (Region.wrap_ghost proj)] in
      return_expr @@ CST.ECall( Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.find_opt"), arg))
    | _ ->
      let field_path = list_to_nsepseq @@ List.map ~f:decompile_to_selection path in
       let struct_name = decompile_variable @@ get_e_variable record in
      let proj : CST.projection = {struct_name;selector=Wrap.ghost "";field_path} in
      return_expr @@ CST.EProj (Region.wrap_ghost proj)
    )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let record = decompile_expression record in
    let (record,updates) = match record with
      CST.EUpdate {value;_} -> (value.record,value.updates)
    | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let var,path = match path with
      Access_record var::path -> (var,path)
    | _ -> failwith "Impossible case %a"
    in
    let field_path = decompile_to_path (Location.wrap @@ Var.of_name var) path in
    let field_expr = decompile_expression update in
    let field_assign : CST.field_path_assignment = {field_path;assignment=Wrap.ghost "";field_expr} in
    let updates = updates.value.ne_elements in
    let updates = Region.wrap_ghost @@ ne_inject ~attr:[] braces @@ npseq_cons (Region.wrap_ghost field_assign) updates in
    let update : CST.update = {lbrace=Wrap.ghost "";record;kwd_with=Wrap.ghost "";updates;rbrace=Wrap.ghost ""} in
    return_expr @@ CST.EUpdate (Region.wrap_ghost update)
  | E_update {record; path; update} ->
    let record = decompile_variable @@ get_e_variable record in
    let field_expr = decompile_expression update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (Region.wrap_ghost name) in
        let update : CST.field_path_assignment = {field_path;assignment=Wrap.ghost "";field_expr} in
        let updates = Region.wrap_ghost @@ ne_inject ~attr:[] braces @@ (Region.wrap_ghost update,[]) in
        let update : CST.update = {lbrace=Wrap.ghost "";record;kwd_with=Wrap.ghost "";updates;rbrace=Wrap.ghost ""} in
        return_expr @@ CST.EUpdate (Region.wrap_ghost update)
      | Access_tuple i ->
        let record : CST.path = Name record in
        let field_path = CST.Name (Region.wrap_ghost @@ Z.to_string i) in
        let update : CST.field_path_assignment = {field_path;assignment=Wrap.ghost "";field_expr} in
        let updates = Region.wrap_ghost @@ ne_inject ~attr:[] braces @@ (Region.wrap_ghost update,[]) in
        let update : CST.update = {lbrace=Wrap.ghost "";record;kwd_with=Wrap.ghost "";updates;rbrace=Wrap.ghost ""} in
        return_expr @@ CST.EUpdate (Region.wrap_ghost update)
      | Access_map e ->
        let e = decompile_expression e in
        let arg = field_expr,[e; CST.EVar record] in
        return_expr @@ CST.ECall (Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.add"), arg))
      )
    | _ ->
      let struct_name = match struct_name with
          Access_record name -> Region.wrap_ghost name
        | Access_tuple i -> Region.wrap_ghost @@ Z.to_string i
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let field_path = List.map ~f:decompile_to_selection field_path in
        let field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=Wrap.ghost "";field_path} in
        let field_path = CST.EProj (Region.wrap_ghost field_path) in
        let e = decompile_expression e in
        let arg = field_expr, [e; field_path] in
        return_expr @@ CST.ECall (Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.add"),arg))
      | _ ->
        let field_path = List.map ~f:decompile_to_selection field_path in
        let field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=Wrap.ghost "";field_path} in
        let field_path = CST.Path (Region.wrap_ghost field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=Wrap.ghost "";field_expr} in
        let updates = Region.wrap_ghost @@ ne_inject ~attr:[] braces @@ (Region.wrap_ghost update,[]) in
        let update : CST.update = {lbrace=Wrap.ghost "";record;kwd_with=Wrap.ghost "";updates;rbrace=Wrap.ghost ""} in
        return_expr @@ CST.EUpdate (Region.wrap_ghost update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let expr = decompile_expression anno_expr in
    let ty   = decompile_type_expr type_annotation in
    return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (expr,Wrap.ghost "",ty))
  | E_module_accessor {module_name;element} ->
    let module_name = Region.wrap_ghost module_name in
    let field  = decompile_expression element in
    return_expr @@ CST.EModA (Region.wrap_ghost CST.{module_name;selector=Wrap.ghost "";field})
  | E_cond {condition;then_clause;else_clause} ->
    let test  = decompile_expression condition in
    let ifso  = decompile_expression then_clause in
    let ifnot = decompile_expression else_clause in
    let ifnot = Some(Wrap.ghost "",ifnot) in
    let cond : CST.cond_expr = {kwd_if=Wrap.ghost "";test;kwd_then=Wrap.ghost "";ifso;ifnot} in
    return_expr @@ CST.ECond (Region.wrap_ghost cond)
  | E_sequence {expr1;expr2} ->
    let expr1 = decompile_expression expr1 in
    let expr2 = decompile_expression expr2 in
    return_expr @@ CST.ESeq (Region.wrap_ghost @@ inject beginEnd @@ list_to_sepseq [expr1; expr2])
  | E_tuple tuple ->
    let tuple = List.map ~f:decompile_expression tuple in
    let tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (Region.wrap_ghost tuple)
  | E_map map ->
    let map = List.map ~f:(Pair.map ~f:decompile_expression) map in
    let aux (k,v) = CST.ETuple (Region.wrap_ghost (k,[(Wrap.ghost "",v)])) in
    let map = List.map ~f:aux map in
    (match map with
      [] -> return_expr @@ CST.EVar (Region.wrap_ghost "Big_map.empty")
    | _  ->
      let var = CST.EVar (Region.wrap_ghost "Map.literal") in
      return_expr @@ CST.ECall (Region.wrap_ghost (var, List.Ne.of_list @@ map))
    )
  | E_big_map big_map ->
    let big_map = List.map ~f:(Pair.map ~f:decompile_expression) big_map in
    let aux (k,v) = CST.ETuple (Region.wrap_ghost (k,[(Wrap.ghost "",v)])) in
    let big_map = List.map ~f:aux big_map in
    (match big_map with
      [] -> return_expr @@ CST.EVar (Region.wrap_ghost "Big_map.empty")
    | _  ->
      let var = CST.EVar (Region.wrap_ghost "Big_map.literal") in
      return_expr @@ CST.ECall (Region.wrap_ghost (var, List.Ne.of_list @@ big_map))
    )
  | E_list lst ->
    let lst = List.map ~f:decompile_expression lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (Region.wrap_ghost @@ inject brackets @@ lst))
  | E_set set ->
    let set = List.map ~f:decompile_expression set in
    let set = List.Ne.of_list @@ set in
    let var = CST.EVar (Region.wrap_ghost "Set.literal") in
    return_expr @@ CST.ECall (Region.wrap_ghost (var,set))
    (* We should avoid to generate skip instruction*)
  | E_skip -> return_expr @@ CST.EUnit (Region.wrap_ghost (Wrap.ghost "",Wrap.ghost ""))
  | E_assign _
  | E_for _
  | E_for_each _
  | E_while _ ->
    failwith @@ Format.asprintf "Decompiling a imperative construct to CameLIGO %a"
    AST.PP.expression expr

and decompile_to_path : AST.expression_variable -> _ AST.access list -> CST.path = fun var access ->
  let struct_name = decompile_variable var.wrap_content in
  match access with
    [] -> CST.Name struct_name
  | lst ->
    let field_path = list_to_nsepseq @@ List.map ~f:decompile_to_selection lst in
    let path : CST.projection = {struct_name;selector=Wrap.ghost "";field_path} in
    (CST.Path (Region.wrap_ghost path) : CST.path)

and decompile_to_selection : _ AST.access -> CST.selection = fun access ->
  match access with
    Access_tuple index -> CST.Component (Region.wrap_ghost ("",index))
  | Access_record str  -> CST.FieldName (Region.wrap_ghost str)
  | Access_map _ ->
    failwith @@ Format.asprintf
    "Can't decompile access_map to selection"

and decompile_lambda : (AST.expr,AST.ty_expr) AST.lambda -> _ = fun {binder;output_type;result} ->
    let param_decl = pattern_type binder in
    let param = (param_decl, []) in
    let ret_type = Option.map ~f:(prefix_colon <@ decompile_type_expr) output_type in
    let result = decompile_expression result in
    (param,ret_type,None,result)

and decompile_declaration : AST.declaration Location.wrap -> CST.declaration = fun decl ->
  let decl = Location.unwrap decl in
  match decl with
    Declaration_type {type_binder;type_expr; type_attr=_} -> (
    let name = decompile_variable type_binder in
    let params =
      match type_expr.type_content with
      | T_abstraction _ -> (
        let rec aux : AST.type_expression -> _ list -> _ list  =
          fun t lst ->
            match t.type_content with
            | T_abstraction x -> aux x.type_ (x.ty_binder::lst)
            | _ -> lst
        in
        let vars = aux type_expr [] in
        let params = type_vars_of_list @@
          List.map ~f:(fun x -> decompile_variable x.wrap_content) vars
        in
        Some params
      )
      | _ -> None
    in
    let type_expr = decompile_type_expr type_expr in
    CST.TypeDecl (Region.wrap_ghost (CST.{kwd_type=Wrap.ghost "";params;name; eq=Wrap.ghost ""; type_expr}))
  )
  | Declaration_constant {binder;attr;expr}-> (
    let attributes : CST.attributes = decompile_attributes attr in
    let var_attributes = binder.attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> decompile_attributes in
    let var = CST.PVar (Region.wrap_ghost ({variable = decompile_variable binder.var.wrap_content; attributes = var_attributes } : CST.var_pattern)) in
    let binders = (var,[]) in
    let type_params, lhs_type = Option.value_map binder.ascr ~default:(None, None)
                                           ~f:(fun t -> let type_params, lhs_type = decompile_type_params t in
                                                        type_params, Some (prefix_colon lhs_type)) in
    match expr.expression_content with
      E_lambda lambda ->
      let let_rhs = decompile_expression (AST.make_e (AST.E_lambda lambda)) in
      let let_binding : CST.let_binding = {binders;type_params;lhs_type;eq=Wrap.ghost "";let_rhs} in
      let let_decl : CST.let_decl = (Wrap.ghost "",None,let_binding,attributes) in
      CST.Let (Region.wrap_ghost let_decl)
    | E_recursive {lambda; _} ->
      let let_rhs = decompile_expression (AST.make_e (AST.E_lambda lambda)) in
      let let_binding : CST.let_binding = {binders;type_params;lhs_type;eq=Wrap.ghost "";let_rhs} in
      let let_decl : CST.let_decl = (Wrap.ghost "",Some Wrap.ghost "",let_binding,attributes) in
      CST.Let (Region.wrap_ghost let_decl)
    | _ ->
      let let_rhs = decompile_expression expr in
      let let_binding : CST.let_binding = {binders;type_params;lhs_type;eq=Wrap.ghost "";let_rhs} in
      let let_decl : CST.let_decl = (Wrap.ghost "",None,let_binding,attributes) in
      CST.Let (Region.wrap_ghost let_decl)
  )
  | Declaration_module {module_binder;module_} ->
    let name = wrap module_binder in
    let module_ = decompile_module module_ in
    let module_decl :CST.module_decl = {kwd_module=Wrap.ghost "";name;eq=Wrap.ghost "";kwd_struct=Wrap.ghost "";module_;kwd_end=Wrap.ghost ""} in
    CST.ModuleDecl (Region.wrap_ghost module_decl)
  | Module_alias {alias;binders} ->
    let alias   = wrap alias in
    let binders = nelist_to_npseq @@ List.Ne.map wrap binders in
    let module_alias :CST.module_alias = {kwd_module=Wrap.ghost "";alias;eq=Wrap.ghost "";binders} in
    CST.ModuleAlias (Region.wrap_ghost module_alias)

and decompile_pattern : AST.type_expression AST.pattern -> CST.pattern =
  fun pattern ->
    match pattern.wrap_content with
    | AST.P_unit -> CST.PUnit (Region.wrap_ghost (Wrap.ghost "", Wrap.ghost ""))
    | AST.P_var v ->
      let name = (decompile_variable v.var.wrap_content).value in
      let attributes = v.attributes |> Tree_abstraction_shared.Helpers.strings_of_binder_attributes `CameLIGO |> decompile_attributes in
      let pvar : CST.var_pattern = {variable = Region.wrap_ghost name; attributes } in
      CST.PVar (Region.wrap_ghost pvar)
    | AST.P_list pl -> (
      let ret x = (CST.PList x) in
      match pl with
      | AST.Cons (pa,pb) ->
        let pa = decompile_pattern pa in
        let pb = decompile_pattern pb in
        let cons = Region.wrap_ghost (pa,Wrap.ghost "",pb) in
        ret (PCons cons)
      | AST.List [] ->
        let nil = list_to_sepseq [] in
        let injection = Region.wrap_ghost @@ inject (brackets) nil in
        ret (PListComp injection)
      | AST.List plst ->
        let plst = List.map ~f:decompile_pattern plst in
        let plst = list_to_sepseq plst in
        let injection = Region.wrap_ghost @@ inject (brackets) plst in
        ret (PListComp injection)
    )
    | AST.P_variant (AST.Label constructor, p) ->
        let p = decompile_pattern p in
        let constr = Region.wrap_ghost (Region.wrap_ghost constructor, Some p) in
        CST.PConstr constr
    | AST.P_tuple lst ->
      let pl = List.map ~f:decompile_pattern lst in
      let pl = list_to_nsepseq pl in
      CST.PTuple (Region.wrap_ghost pl)
    | AST.P_record (llst,lst) ->
      let pl = List.map ~f:decompile_pattern lst in
      let fields_name = List.map ~f:(fun (AST.Label x) -> Region.wrap_ghost x) llst in
      let field_patterns =
        List.map
          ~f:(fun (field_name,pattern) -> Region.wrap_ghost ({ field_name ; eq = Wrap.ghost "" ; pattern }:CST.field_pattern))
          (List.zip_exn fields_name pl)
      in
      let field_patterns = list_to_nsepseq field_patterns in
      let inj = ne_inject braces field_patterns ~attr:[] in
      CST.PRecord (Region.wrap_ghost inj)

and decompile_module : AST.module_ -> CST.ast = fun prg ->
  let decl = List.map ~f:decompile_declaration prg in
  let decl = List.Ne.of_list decl in
  ({decl;eof=Wrap.ghost ""}: CST.ast)
