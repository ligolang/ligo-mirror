(* Decompiler to the CST of PascaLIGO *)

module AST  = Ast_imperative
module CST  = Cst.Pascaligo
module Wrap = Lexing_shared.Wrap
module Token = Lexing_pascaligo.Token
module Predefined = Predefined.Tree_abstraction.Pascaligo

open Function

(* Utils *)

let decompile_attributes : 'a -> CST.attributes = fun kvl ->
  ignore kvl;failwith "TODO: change attributes representation in AST"
  (* List.map ~f:Region.wrap_ghost *)
let list_to_sepseq ~sep lst =
  match lst with
    [] -> None
  | hd::tl ->
      let aux e = sep, e in
      Some (hd, List.map ~f:aux tl)
let list_to_nsepseq ~sep lst =
  match list_to_sepseq ~sep lst with
    Some s -> s
  | None   -> failwith "List is not a non_empty list" (* TODO: NO failwith! *)
let nelist_to_npseq (hd, tl) =
  hd, List.map ~f:(fun e -> (Wrap.ghost "", e)) tl
let npseq_cons hd tl = hd, ((Wrap.ghost "", fst tl) :: snd tl)
let par a = CST.{lpar=Token.ghost_lpar; inside=a; rpar=Token.ghost_rpar}
let type_vars_of_list : string Region.reg list -> CST.type_vars =
  fun lst ->
  let lst = List.map lst ~f:(fun sr -> Wrap.ghost sr.value) in
  let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
  Region.wrap_ghost (par lst)
let brackets a = CST.{lbracket=Wrap.ghost "";inside=a;rbracket=Wrap.ghost ""}
let prefix_colon a = (Wrap.ghost "", a)
let suffix_with a = (a, Wrap.ghost "")

(* Dialect-relevant functions *)

type dialect = Terse | Verbose
let terminator = function
  | Terse -> Some Token.ghost_semi
  | Verbose -> None
let lead_vbar = terminator
let enclosing_brackets = CST.Brackets (Token.ghost_lbracket, Token.ghost_rbracket)
let enclosing = function
  | Terse -> enclosing_brackets
  | Verbose -> CST.End Token.ghost_end
let block_enclosing = function
  | Terse -> CST.Braces (None, Token.ghost_lbrace, Token.ghost_rbrace)
  | Verbose -> CST.BeginEnd (Token.ghost_begin, Token.ghost_end)
let module_enclosing = function
  | Terse -> CST.Braces (None, Token.ghost_lbrace, Token.ghost_rbrace)
  | Verbose -> CST.BeginEnd (Token.ghost_begin, Token.ghost_end)
let inject : dialect -> string Wrap.wrap -> CST.attributes -> ('a, CST.semi) Utils.sepseq -> 'a CST.compound =
  fun dialect kind attributes elements ->
    { kind ;
      enclosing=enclosing dialect ;
      elements ;
      terminator=Some Token.ghost_semi ;
      attributes
    }
let to_block dialect a =
  CST.{enclosing=block_enclosing dialect;statements=a;terminator=terminator dialect}
(* 
let empty_block dialect =
  to_block dialect (CST.Instr (CST.Skip (Wrap.ghost "")),[]) *)

(* Decompiler *)

let decompile_variable : type a. a Var.t -> CST.variable = fun var ->
  let var = Format.asprintf "%a" Var.pp var in
  if String.contains var '#' then
    let var = String.split_on_char '#' var in
    Wrap.ghost @@ "gen__" ^ (String.concat "" var)
  else
    if String.length var > 4 && String.equal "gen__" @@ String.sub var 0 5 then
      Wrap.ghost @@ "user__" ^ var
    else
      Wrap.ghost var
let rec decompile_type_expr : dialect -> AST.type_expression -> CST.type_expr = fun dialect te ->
  let return te = te in
  match te.type_content with
  | T_sum {attributes ; fields } ->
    let attributes = decompile_attributes attributes in
    let lst = AST.LMap.to_kv_list fields in
    let aux (AST.Label c, AST.{associated_type; attributes=row_attr; _}) =
      let ctor = Wrap.ghost c in
      let arg = decompile_type_expr dialect associated_type in
      let args = Some (Token.ghost_of, arg) in
      let attributes : CST.attributes = decompile_attributes row_attr in
      let variant : CST.variant = {ctor ; args; attributes} in
      Region.wrap_ghost variant in
    let variants = List.map ~f:aux lst in
    let variants = list_to_nsepseq ~sep:Token.ghost_vbar variants in
    let lead_vbar = Some Token.ghost_vbar in
    let sum : CST.sum_type = { lead_vbar ; variants ; attributes}in
    return @@ CST.T_Sum (Region.wrap_ghost sum)
  | T_record {fields; attributes} ->
     let record = AST.LMap.to_kv_list fields in
     let aux (AST.Label c, AST.{associated_type; attributes=field_attr; _}) =
    let field_name = Wrap.ghost c in
    let field_type = decompile_type_expr dialect associated_type in
      let field_attr = decompile_attributes field_attr in
       let field : CST.field_decl =
         {field_name; field_type = Some (Token.ghost_colon , field_type); attributes=field_attr} in
       Region.wrap_ghost field in
    let record = List.map ~f:aux record in
    let elements = list_to_sepseq ~sep:(Token.ghost_semi) record in
    let attributes = decompile_attributes attributes in
    let compound : CST.field_decl CST.reg CST.compound =
      { kind = Token.ghost_record ; enclosing = enclosing_brackets ; elements ; terminator = None ; attributes }
    in
    return @@ CST.T_Record (Region.wrap_ghost compound)
  | T_tuple tuple ->
    let lst = List.map ~f:(decompile_type_expr dialect) tuple in
    let (lhs,tail) = List.Ne.of_list lst in
    let tuple : CST.cartesian = Region.wrap_ghost (lhs, Token.ghost_times, list_to_nsepseq ~sep:Token.ghost_times tail) in
    return @@ CST.T_Cart tuple
  | T_arrow {type1;type2} ->
    let type1 = decompile_type_expr dialect type1 in
    let type2 = decompile_type_expr dialect type2 in
    let arrow = (type1, Wrap.ghost "", type2) in
    return @@ CST.T_Fun (Region.wrap_ghost arrow)
  | T_variable variable ->
    let v = decompile_variable variable in
    return @@ CST.T_Var v
  | T_app {type_operator; arguments} ->
    let v = CST.T_Var (decompile_variable type_operator) in
    let lst = List.map ~f:(decompile_type_expr dialect) arguments in
    let lst = list_to_nsepseq ~sep:Token.ghost_comma lst in
    let lst : _ CST.par = {lpar=Wrap.ghost "";inside=lst;rpar=Wrap.ghost ""} in
    return @@ CST.T_App (Region.wrap_ghost (v,Region.wrap_ghost lst))
  | T_annoted _annot ->
    failwith "TODO: decompile T_annoted"
  | T_module_accessor {module_name;element} ->
    let module_path = list_to_nsepseq ~sep:Token.ghost_dot [Wrap.ghost module_name] in
    let field  = decompile_type_expr dialect element in
    return @@ CST.T_ModPath (Region.wrap_ghost CST.{module_path;selector=Token.ghost_dot;field})
  | T_singleton x -> (
    match x with
    | Literal_int i ->
      let z : CST.type_expr = CST.T_Int (Wrap.ghost ("",i)) in
      return z
    | Literal_string s ->
      let z : CST.type_expr = CST.T_String (Wrap.ghost (Ligo_string.extract s)) in
      return z
    | _ -> failwith "unsupported singleton"
  )
  | T_abstraction x -> decompile_type_expr dialect x.type_
  | T_for_all x -> decompile_type_expr dialect x.type_

let get_e_variable : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_variable var -> var.wrap_content
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let rec get_e_accessor : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_variable var -> (var, [])
  | E_accessor {record;path} ->
    let (var, lst) = get_e_accessor record in
    (var, lst @ path)
  | _ -> failwith @@
    Format.asprintf "%a should be a variable expression"
    AST.PP.expression expr

let get_e_tuple : AST.expression -> _ = fun expr ->
  match expr.expression_content with
    E_tuple tuple -> tuple
  | E_variable _
  | E_literal _
  | E_constant _
  | E_lambda _ -> [expr]
  | _ -> failwith @@
    Format.asprintf "%a should be a tuple expression"
    AST.PP.expression expr
type eos =
| Expression
| Statements

let rec decompile_expression ?(dialect=Verbose) : AST.expression -> CST.expr = fun e ->
  let (block,expr) = decompile_to_block dialect e in
  match expr with
  | Some expr -> (
    match block with
    | Some block ->
      let block = Region.wrap_ghost block in
      CST.E_Block (Region.wrap_ghost @@ CST.{block;kwd_with=Wrap.ghost "";expr})
    | None -> expr
  )
  | None ->
    failwith @@ Format.asprintf
      "An expression was expected, but this was decompiled to statements.\
      @.Expr : %a@ @,Loc : %a"
      AST.PP.expression e
      Location.pp e.location

and decompile_statements : dialect -> AST.expression -> _ = fun dialect expr ->
  let (stat,_) = decompile_eos dialect Statements expr in
  match stat with
    Some stat -> stat
  | None ->
      failwith @@ Format.asprintf
        "Statements was expected, but this was decompiled to expression.\
        @.Expr : %a@ @,Loc : %a"
        AST.PP.expression expr
        Location.pp expr.location

and decompile_pattern : dialect -> AST.type_expression AST.pattern -> CST.pattern =
  fun dialect pattern ->
    match pattern.wrap_content with
    | AST.P_unit ->
      CST.P_Ctor (Wrap.ghost "Unit")
    | AST.P_var v ->
      let name = decompile_variable v.var.wrap_content in
      CST.P_Var name
    | AST.P_list pl -> (
      match pl with
      | AST.Cons (pa,pb) ->
        let pa = decompile_pattern dialect pa in
        let pb = decompile_pattern dialect pb in
        let cons = Region.wrap_ghost (pa, Token.ghost_sharp, pb) in
        P_Cons cons
      | AST.List [] -> P_Nil Token.ghost_nil
      | AST.List plst ->
        let plst = List.map ~f:(decompile_pattern dialect) plst in
        let elements = list_to_sepseq ~sep:Token.ghost_semi plst in
        P_List (Region.wrap_ghost (inject dialect Token.ghost_list [] elements))
    )
    | AST.P_variant (constructor,p) -> (
      match constructor with
      | Label constructor -> (
        let p = decompile_pattern dialect p in
        let p = list_to_nsepseq ~sep:Token.ghost_comma [p] in
        let p : CST.tuple_pattern = Region.wrap_ghost (par p) in
        let constr = CST.P_Ctor (Wrap.ghost constructor) in
        CST.P_App (Region.wrap_ghost (constr, Some p))
      )
    )
    | AST.P_tuple lst ->
      let pl = List.map ~f:(decompile_pattern dialect) lst in
      let pl = list_to_nsepseq ~sep:Token.ghost_comma pl in
      CST.P_Tuple (Region.wrap_ghost (par pl))
    | AST.P_record (labels, patterns) ->
      let aux : AST.label * AST.type_expression AST.pattern -> CST.field_pattern CST.reg =
        fun (Label label, pattern) ->
          let field_rhs = decompile_pattern dialect pattern in
          let full_field = CST.Complete {field_lhs = Wrap.ghost label ; assign = Token.ghost_eq ; field_rhs ; attributes = [] } in
          Region.wrap_ghost full_field
      in
      let field_patterns = List.map ~f:aux (List.zip_exn labels patterns) in
      let inj = inject dialect Token.ghost_record [] (list_to_sepseq ~sep:Token.ghost_semi field_patterns) in
      CST.P_Record (Region.wrap_ghost inj)

and decompile_to_block : dialect -> AST.expression -> _ = fun dialect expr ->
  let (stats,next) = decompile_eos dialect Expression expr in
  let block = Option.map ~f:(to_block dialect <@ nelist_to_npseq) stats in
  (block, next)

and decompile_to_tuple_expr : dialect -> AST.expression list -> CST.tuple_expr = fun dialect expr ->
  let tuple_expr = List.map ~f:(decompile_expression ~dialect) expr in
  let tuple_expr = list_to_nsepseq ~sep:Token.ghost_comma tuple_expr in
  let tuple_expr : CST.tuple_expr = Region.wrap_ghost @@ par @@  tuple_expr in
  tuple_expr

and decompile_eos : dialect -> eos -> AST.expression -> ((CST.statement List.Ne.t option)* CST.expr option) = fun dialect output expr ->
  let return (a,b) = (a,b) in
  let return_expr expr = return @@ (None, Some expr) in
  let return_expr_with_par expr = return_expr @@ CST.E_Par (Region.wrap_ghost @@ par @@ expr) in
  let return_typed expr ty = return_expr @@ CST.E_Typed (Region.wrap_ghost @@ par (expr,(Token.ghost_colon, ty))) in
  let return_stat stat = return @@ (Some stat, None) in
  let return_stat_ez stat = return_stat @@ (stat, []) in
  let return_inst inst = return_stat_ez @@ CST.S_Instr inst in
  match expr.expression_content with
    E_variable name ->
    let var = decompile_variable name.wrap_content in
    return_expr @@ CST.E_Var var
  | E_constant {cons_name; arguments} -> (
    let expr = CST.E_Var (Wrap.ghost (Predefined.constant_to_string cons_name)) in
    match arguments with
      [] -> return_expr @@ expr
    | _ -> (
      let arguments = decompile_to_tuple_expr dialect arguments in
      let const : (CST.expr * CST.arguments) CST.reg = Region.wrap_ghost (expr, arguments) in
      match output with
      | Expression -> return_expr (CST.E_Call const)
      | Statements -> return_inst (CST.I_Call const)
    )
  )
  | E_literal literal -> (
    match literal with
    | Literal_unit  ->  return_expr @@ CST.E_Ctor (Wrap.ghost "Unit")
    | Literal_int i ->  return_expr @@ CST.E_Int (Token.ghost_int i)
    | Literal_nat n ->  return_expr @@ CST.E_Nat (Token.ghost_nat n)
    | Literal_timestamp time ->
      let time = Tezos_utils.Time.Protocol.to_notation @@
        Tezos_utils.Time.Protocol.of_seconds @@ Z.to_int64 time
      in
      let ty = decompile_type_expr dialect @@ AST.t_timestamp () in
      let time = CST.E_String (Wrap.ghost time) in
      return_typed time ty
    | Literal_mutez mtez -> return_expr @@ CST.E_Mutez (Token.ghost_mutez (Z.to_int64 mtez))
    | Literal_string (Standard str) -> return_expr @@ CST.E_String (Wrap.ghost str)
    | Literal_string (Verbatim ver) -> return_expr @@ CST.E_Verbatim (Wrap.ghost ver)
    | Literal_bytes b ->
      let b = Hex.of_bytes b in
      return_expr @@ CST.E_Bytes (Token.ghost_bytes b)
    | Literal_address addr ->
      let addr = CST.E_String (Wrap.ghost addr) in
      let ty = decompile_type_expr dialect @@ AST.t_address () in
      return_typed addr ty
    | Literal_signature sign ->
      let sign = CST.E_String (Wrap.ghost sign) in
      let ty = decompile_type_expr dialect @@ AST.t_signature () in
      return_typed sign ty
    | Literal_key k ->
      let k = CST.E_String (Wrap.ghost k) in
      let ty = decompile_type_expr dialect @@ AST.t_key () in
      return_typed k ty
    | Literal_key_hash kh ->
      let kh = CST.E_String (Wrap.ghost kh) in
      let ty = decompile_type_expr dialect @@ AST.t_key_hash () in
      return_typed kh ty
    | Literal_chain_id _
    | Literal_operation _ ->
      failwith "chain_id, operation are not created currently ?"
  )
  | E_application {lamb;args} ->
    let lamb = decompile_expression ~dialect lamb in
    let args = (decompile_to_tuple_expr dialect) @@ get_e_tuple args in
    (match output with
      Expression ->
      return_expr @@ CST.E_Call (Region.wrap_ghost (lamb,args))
    | Statements ->
      return_inst @@ CST.I_Call (Region.wrap_ghost (lamb,args))
    )
  | E_lambda lambda ->
    let (param,ret_type,return) = decompile_lambda dialect lambda in
    let fun_expr : CST.fun_expr = {kwd_function=Wrap.ghost "";param;ret_type;kwd_is=Wrap.ghost "";return;attributes=[]} in
    return_expr_with_par @@ CST.E_Fun (Region.wrap_ghost @@ fun_expr)
  | E_recursive _ ->
    failwith "corner case : annonymous recursive function"
  | E_let_in {let_binder;rhs;let_result;attributes} ->
    let lin = decompile_to_data_decl dialect let_binder rhs attributes in
    let (lst, expr) = decompile_eos dialect Expression let_result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.S_Decl lin) lst
    | None -> (CST.S_Decl lin, [])
    in
    return @@ (Some lst, expr)
  | E_type_in {type_binder;rhs;let_result} ->
    let kwd_type = Token.ghost_type
    and name = decompile_variable type_binder
    and kwd_is = Token.ghost_is in
    let type_expr = decompile_type_expr dialect rhs in
    let terminator = terminator dialect in
    let tin = Region.wrap_ghost @@ (CST.{kwd_type; name; kwd_is; type_expr; terminator ; params = None}) in
    let (lst, expr) = decompile_eos dialect Expression let_result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.S_Decl (CST.D_Type tin)) lst
    | None -> (CST.S_Decl (CST.D_Type tin), [])
    in
    return @@ (Some lst, expr)
  | E_mod_in {module_binder;rhs;let_result} ->
    let kwd_module = Token.ghost_module
    and name = Wrap.ghost module_binder
    and kwd_is = Token.ghost_is in
    let declarations = decompile_module ~dialect rhs in
    let terminator = terminator dialect in
    let enclosing  = module_enclosing dialect in
    let min = Region.wrap_ghost @@ (CST.{kwd_module; name; kwd_is;enclosing; declarations; terminator}) in
    let (lst, expr) = decompile_eos dialect Expression let_result in
    let lst =
      match lst with
      | Some lst -> List.Ne.cons (CST.S_Decl (CST.D_Module min)) lst
      | None -> (CST.S_Decl (CST.D_Module min), [])
    in
    return @@ (Some lst, expr)
  | E_mod_alias {alias; binders; result} ->
    let alias = Wrap.ghost alias in
    let mod_path = nelist_to_npseq @@ List.Ne.map Wrap.ghost binders in
    let terminator = terminator dialect in
    let ma : CST.module_alias CST.reg = Region.wrap_ghost @@
      CST.{ kwd_module=Token.ghost_module ; alias ; kwd_is=Token.ghost_is ; mod_path ; terminator }
    in
    let (lst, expr) = decompile_eos dialect Expression result in
    let lst = match lst with
      Some lst -> List.Ne.cons (CST.S_Decl (CST.D_ModAlias ma)) lst
    | None -> (CST.S_Decl (CST.D_ModAlias ma), [])
    in
    return @@ (Some lst, expr)
  | E_raw_code {language; code} ->
    let language = Region.wrap_ghost @@ Region.wrap_ghost @@ language in
    let code = decompile_expression ~dialect code in
    let ci : CST.code_inj = {language;code;rbracket=Token.ghost_rbracket} in
    return_expr @@ CST.E_CodeInj (Region.wrap_ghost ci)
  | E_constructor {constructor;element} ->
    let Label constr = constructor in
    let constr = Wrap.ghost constr in
    let element = decompile_to_tuple_expr dialect @@ get_e_tuple element in
    return_expr_with_par @@ CST.E_App (Region.wrap_ghost (CST.E_Ctor constr, Some element))
  | E_matching {matchee; cases} -> (
    let expr  = decompile_expression ~dialect matchee in
    let enclosing = enclosing dialect in
    let lead_vbar = lead_vbar dialect in
    let aux decompile_f =
      fun ({ pattern ; body }:(AST.expression, AST.type_expression) AST.match_case) ->
        let pattern = decompile_pattern dialect pattern in
        let rhs = decompile_f body in
        let clause : (_ CST.case_clause)= { pattern ; arrow = Token.ghost_arrow ; rhs } in
        (Region.wrap_ghost clause)
    in
    match output with
    | Expression ->
      let cases = List.map ~f:(aux (decompile_expression ~dialect)) cases in
      let cases = list_to_nsepseq ~sep:Token.ghost_vbar cases in
      let cases : _ CST.case = {kwd_case=Token.ghost_case;expr;kwd_of=Token.ghost_of;enclosing;lead_vbar;cases} in
      return_expr @@ CST.E_Case (Region.wrap_ghost cases)
    | Statements ->
      let cases = List.map ~f:(aux (decompile_if_clause dialect)) cases in
      let cases : (CST.test_clause CST.case_clause CST.reg, CST.vbar) Utils.nsepseq = list_to_nsepseq ~sep:Token.ghost_vbar cases in
      let cases : CST.test_clause CST.case = {kwd_case=Token.ghost_case;expr;kwd_of=Token.ghost_of;enclosing;lead_vbar;cases} in
      return_inst @@ CST.I_Case (Region.wrap_ghost cases)
  )
  | E_record record  ->
    let record = AST.LMap.to_kv_list record in
    let aux (AST.Label str, expr) =
      let field_name = Wrap.ghost str in
      let field_rhs = decompile_expression ~dialect expr in
      let field : (CST.expr , CST.expr) CST.field =
        Complete {field_lhs = E_Var field_name ; assign=Token.ghost_ass;field_rhs;attributes=[]}
      in
      Region.wrap_ghost field
    in
    let record = List.map ~f:aux record in
    let record = list_to_sepseq ~sep:Token.ghost_semi record in
    let record = inject dialect Token.ghost_record [] record in
    return_expr @@ CST.E_Record (Region.wrap_ghost record)
  | E_accessor {record; path} -> (
    match List.rev path with
      Access_map e :: [] ->
      let (var,lst) = get_e_accessor @@ record in
      let map = decompile_to_path var lst in
      let e = decompile_expression ~dialect e in
      let index = Region.wrap_ghost @@ brackets @@ e in
      let mlu : CST.map_lookup = {map;index} in
      return_expr @@ CST.E_MapLookup (Region.wrap_ghost @@ mlu)
    | Access_map e :: lst ->
      let path = List.rev lst in
      let field_path = list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection path in
      let struct_name = CST.E_Var (decompile_variable @@ get_e_variable record) in
      let proj = CST.{record_or_tuple = struct_name ; selector=Token.ghost_dot ; field_path} in
      let map = CST.E_Proj (Region.wrap_ghost proj) in
      let e = decompile_expression ~dialect e in
      let index = Region.wrap_ghost @@ brackets @@ e in
      let mlu : CST.map_lookup = {map;index} in
      return_expr @@ CST.E_MapLookup (Region.wrap_ghost @@ mlu)
    | _ ->
      let field_path = list_to_nsepseq ~sep:Token.ghost_dot @@ List.map ~f:decompile_to_selection path in
      let record_or_tuple = CST.E_Var (decompile_variable @@ get_e_variable record) in
      let proj = CST.{record_or_tuple ; selector=Token.ghost_dot ; field_path} in
      return_expr @@ CST.E_Proj (Region.wrap_ghost proj)
  )
  (* Update on multiple field of the same record. may be removed by adding sugar *)
  | E_update {record={expression_content=E_update _;_} as record;path;update} ->
    let record = decompile_expression ~dialect record in
    let (record,updates) =
      match record with
      | CST.E_Update {value;_} -> (value.structure,value.update)
      | _ -> failwith @@ Format.asprintf "Inpossible case %a" AST.PP.expression expr
    in
    let var,path =
      match path with
      | Access_record var::path -> (var,path)
      | _ -> failwith "Impossible case %a"
    in
    let field_path = decompile_to_path (Location.wrap @@ Var.of_name var) path in
    let field_expr = decompile_expression ~dialect update in
    let field_assign : CST.update = {field_path ; assignment=Token.ghost_with ; field_expr} in
    let updates = updates.value.ne_elements in
    let updates = Region.wrap_ghost @@ inject ~attr:[] dialect (NEInjRecord (Wrap.ghost ""))
                  @@ npseq_cons (Region.wrap_ghost @@ field_assign) updates in
    let update : CST.update = {record;kwd_with=Wrap.ghost "";updates} in
    return_expr @@ CST.E_Update (Region.wrap_ghost @@ update)
  | E_update {record; path; update} ->
    let record = decompile_variable @@ get_e_variable record in
    let field_expr = decompile_expression ~dialect update in
    let (struct_name,field_path) = List.Ne.of_list path in
    (match field_path with
      [] ->
      (match struct_name with
        Access_record name ->
        let record : CST.path = Name record in
        let field_path = CST.Name (Region.wrap_ghost name) in
        let update : CST.field_path_assignment = {field_path;assignment=Wrap.ghost "";field_expr} in
        let updates = Region.wrap_ghost @@ inject ~attr:[] dialect (NEInjRecord (Wrap.ghost "")) @@ (Region.wrap_ghost update,[]) in
        let update : CST.update = {record;kwd_with=Wrap.ghost "";updates;} in
        return_expr @@ CST.EUpdate (Region.wrap_ghost update)
      | Access_tuple _ -> failwith @@ Format.asprintf "invalid tuple update %a" AST.PP.expression expr
      | Access_map e ->
        let e = decompile_expression ~dialect e in
        let arg : CST.tuple_expr = Region.wrap_ghost @@ par @@ nelist_to_npseq (field_expr,[e; CST.EVar record]) in
        return_expr @@ CST.ECall (Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.add"), arg))
      )
    | _ ->
      let struct_name = match struct_name with
          Access_record name -> Region.wrap_ghost name
        | Access_tuple _ -> failwith @@ Format.asprintf "invalid tuple update %a" AST.PP.expression expr
        | Access_map _ -> failwith @@ Format.asprintf "invalid map update %a" AST.PP.expression expr
      in
      (match List.rev field_path with
        Access_map e :: lst ->
        let field_path = List.rev lst in
        let field_path = List.map ~f:decompile_to_selection field_path in
        let field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=Wrap.ghost "";field_path} in
        let field_path = CST.EProj (Region.wrap_ghost @@ field_path) in
        let e = decompile_expression ~dialect e in
        let arg = Region.wrap_ghost @@ par @@ nelist_to_npseq (field_expr, [e; field_path]) in
        return_expr @@ CST.ECall (Region.wrap_ghost (CST.EVar (Region.wrap_ghost "Map.add"),arg))
      | _ ->
        let field_path = List.map ~f:decompile_to_selection field_path in
        let field_path = list_to_nsepseq field_path in
        let field_path : CST.projection = {struct_name; selector=Wrap.ghost "";field_path} in
        let field_path : CST.path = CST.Path (Region.wrap_ghost @@ field_path) in
        let record : CST.path = Name record in
        let update : CST.field_path_assignment = {field_path;assignment=Wrap.ghost "";field_expr} in
        let updates = Region.wrap_ghost @@ inject ~attr:[] dialect (NEInjRecord (Wrap.ghost "")) @@ (Region.wrap_ghost update,[]) in
        let update : CST.update = {record;kwd_with=Wrap.ghost "";updates;} in
        return_expr @@ CST.EUpdate (Region.wrap_ghost update)
      )
    )
  | E_ascription {anno_expr;type_annotation} ->
    let expr = decompile_expression ~dialect anno_expr in
    let ty   = decompile_type_expr dialect type_annotation in
    return_expr @@ CST.EAnnot (Region.wrap_ghost @@ par (expr,Wrap.ghost "",ty))
  | E_module_accessor {module_name;element} ->
    let module_name = Region.wrap_ghost module_name in
    let field  = decompile_expression element in
    return_expr @@ CST.EModA (Region.wrap_ghost CST.{module_name;selector=Wrap.ghost "";field})
  | E_cond {condition;then_clause;else_clause} ->
     let test  = decompile_expression ~dialect condition in
     let terminator = terminator dialect in
    (match output with
      Expression ->
      let ifso = decompile_expression ~dialect then_clause in
      let ifnot = decompile_expression ~dialect else_clause in
      let cond : CST.cond_expr = {kwd_if=Wrap.ghost "";test;kwd_then=Wrap.ghost "";ifso;terminator;kwd_else=Wrap.ghost "";ifnot} in
      return_expr @@ CST.ECond (Region.wrap_ghost cond)
    | Statements ->
      let ifso  = decompile_if_clause dialect then_clause in
      let ifnot = decompile_if_clause dialect else_clause in
      let cond : CST.conditional = {kwd_if=Wrap.ghost "";test;kwd_then=Wrap.ghost "";ifso;terminator; kwd_else=Wrap.ghost "";ifnot} in
      return_inst @@ CST.Cond (Region.wrap_ghost cond)
    )
  | E_sequence {expr1;expr2} ->
    let expr1 = decompile_statements dialect expr1 in
    let (expr2,next) = decompile_eos dialect output expr2 in
    let expr1 = Option.value ~default:expr1 @@ Option.map ~f:(List.Ne.append expr1) expr2 in
    return @@ (Some expr1, next)
  | E_skip -> return_inst @@ CST.Skip (Wrap.ghost "")
  | E_tuple tuple ->
    let tuple = List.map ~f:(decompile_expression ~dialect) tuple in
    let tuple = list_to_nsepseq tuple in
    return_expr @@ CST.ETuple (Region.wrap_ghost @@ par tuple)
  | E_map map ->
    let map = List.map ~f:(Pair.map ~f:(decompile_expression ~dialect)) map in
    let aux (k,v) =
      let binding : CST.binding = {source=k;arrow=Wrap.ghost "";image=v} in
      Region.wrap_ghost @@ binding
    in
    let map = list_to_sepseq @@ List.map ~f:aux map in
    return_expr @@ CST.EMap (MapInj (Region.wrap_ghost @@ inject dialect (InjMap (Wrap.ghost "")) @@ map))
  | E_big_map big_map ->
    let big_map = List.map ~f:(Pair.map ~f:(decompile_expression ~dialect)) big_map in
    let aux (k,v) =
      let binding : CST.binding = {source=k;arrow=Wrap.ghost "";image=v} in
      Region.wrap_ghost @@ binding
    in
    let big_map = list_to_sepseq @@ List.map ~f:aux big_map in
    return_expr @@ CST.EMap (BigMapInj (Region.wrap_ghost @@ inject dialect (InjBigMap (Wrap.ghost "")) @@ big_map))
  | E_list lst ->
    let lst = List.map ~f:(decompile_expression ~dialect) lst in
    let lst = list_to_sepseq lst in
    return_expr @@ CST.EList (EListComp (Region.wrap_ghost @@ inject dialect (InjList (Wrap.ghost "")) @@ lst))
  | E_set set ->
    let set = List.map ~f:(decompile_expression ~dialect) set in
    let set = list_to_sepseq set in
    return_expr @@ CST.ESet (SetInj (Region.wrap_ghost @@ inject dialect (InjSet (Wrap.ghost "")) @@ set))
  | E_assign {variable;access_path;expression} ->
    let lhs = decompile_to_lhs dialect variable access_path in
    let rhs = decompile_expression ~dialect expression in
    let assign : CST.assignment = {lhs;assign=Wrap.ghost "";rhs} in
    return_inst @@ Assign (Region.wrap_ghost assign)
  | E_for {binder;start;final;incr;f_body} ->
    let binder     = decompile_variable binder.wrap_content in
    let init  = decompile_expression ~dialect start in
    let bound = decompile_expression ~dialect final in
    let step  = decompile_expression ~dialect incr  in
    let step       = Some (Wrap.ghost "", step) in
    let (block,_next) = decompile_to_block dialect f_body in
    let block = Region.wrap_ghost @@ Option.value ~default:(empty_block dialect) block in
    let fl : CST.for_int = {kwd_for=Wrap.ghost "";binder;assign=Wrap.ghost "";init;kwd_to=Wrap.ghost "";bound;step;block} in
    return_inst @@ CST.Loop (For (ForInt (Region.wrap_ghost fl)))
  | E_for_each {fe_binder;collection;collection_type;fe_body} ->
    let var = decompile_variable @@ (fst fe_binder).wrap_content in
    let bind_to = Option.map ~f:(fun (x:AST.expression_variable) -> (Wrap.ghost "",decompile_variable x.wrap_content)) @@ snd fe_binder in
    let expr = decompile_expression ~dialect collection in
    let collection = match collection_type with
        Map -> CST.Map (Wrap.ghost "")
      | Set -> Set (Wrap.ghost "")
      | List -> List (Wrap.ghost "")
      | Any -> failwith "TODO : have the type of the collection propagated from AST_typed" in
    let (block,_next) = decompile_to_block dialect fe_body in
    let block = Region.wrap_ghost @@ Option.value ~default:(empty_block dialect) block in
    let fc : CST.for_collect = {kwd_for=Wrap.ghost "";var;bind_to;kwd_in=Wrap.ghost "";collection;expr;block} in
    return_inst @@ CST.Loop (For (ForCollect (Region.wrap_ghost fc)))
  | E_while {cond;body} ->
    let cond  = decompile_expression ~dialect cond in
    let (block,_next) = decompile_to_block dialect body in
    let block = Region.wrap_ghost @@ Option.value ~default:(empty_block dialect) block in
    let loop : CST.while_loop = {kwd_while=Wrap.ghost "";cond;block} in
    return_inst @@ CST.Loop (While (Region.wrap_ghost loop))

and decompile_if_clause : dialect -> AST.expression -> CST.test_clause = fun dialect e ->
  let clause = decompile_statements dialect e in
  match clause with
    CST.Instr instr,[] ->
    CST.ClauseInstr instr
  | _ ->
    let clause = nelist_to_npseq clause, Some (Wrap.ghost "") in
    CST.ClauseBlock (ShortBlock (Region.wrap_ghost @@ braces clause))

and decompile_to_data_decl : dialect -> _ AST.binder -> AST.expression -> AST.attributes -> CST.declaration =
    fun dialect binder expr attributes ->
  let name = decompile_variable binder.var.wrap_content in
  let const_type =
    Option.map
      ~f:(prefix_colon <@ decompile_type_expr dialect)
      binder.ascr in
  let attributes = decompile_attributes attributes in
  let fun_name = name in
  let terminator = terminator dialect in
  match expr.expression_content with
    E_lambda lambda ->
    let (param,ret_type,return) = decompile_lambda dialect lambda in
    let fun_decl : CST.fun_decl = {kwd_recursive=None;kwd_function=Wrap.ghost "";fun_name;param;ret_type;kwd_is=Wrap.ghost "";return;terminator;attributes} in
    CST.LocalFun (Region.wrap_ghost fun_decl)
  | E_recursive {lambda; _} ->
    let (param,ret_type,return) = decompile_lambda dialect lambda in
    let fun_decl : CST.fun_decl = {
        kwd_recursive = Some (Wrap.ghost "");
        kwd_function=Wrap.ghost "";
        fun_name;
        param;
        ret_type;
        kwd_is=Wrap.ghost "";
        return;
        terminator;
        attributes} in
    CST.LocalFun (Region.wrap_ghost fun_decl)
  | _ ->
    let init = decompile_expression ~dialect expr in
    let pattern = CST.PVar (Region.wrap_ghost ({variable=name ; attributes = []}: CST.var_pattern)) in
    let const_decl : CST.const_decl = {kwd_const=Wrap.ghost "" ; pattern ;const_type;equal=Wrap.ghost "";init;terminator; attributes} in
    let data_decl  : CST.data_decl  =  LocalConst (Region.wrap_ghost const_decl) in
    data_decl

and decompile_to_lhs : dialect -> AST.expression_variable -> _ AST.access list -> CST.expr = fun dialect var access ->
  match List.rev access with
    [] -> E_Var (decompile_variable var.wrap_content)
  | hd :: tl ->
    match hd with
    | AST.Access_map e ->
      let path = decompile_to_path var @@ List.rev tl in
      let index = (Region.wrap_ghost <@ brackets) @@ decompile_expression ~dialect e in
      let mlu: CST.map_lookup = {path;index} in
      CST.MapPath (Region.wrap_ghost @@ mlu)
    | Access_tuple z | Access_record str ->
      let path = decompile_to_path var @@ access in
      (CST.Path (path) : CST.lhs)

and decompile_to_path : AST.expression_variable -> _ AST.access list -> CST.expr = fun var access ->
  let struct_name = decompile_variable var.wrap_content in
  match access with
    [] -> CST.Name struct_name
  | lst ->
    let field_path = list_to_nsepseq @@ List.map ~f:decompile_to_selection lst in
    let path : CST.projection = {struct_name;selector=Wrap.ghost "";field_path} in
    (CST.Path (Region.wrap_ghost @@ path) : CST.path)

and decompile_to_selection : _ AST.access -> CST.selection = fun access ->
  match access with
    Access_tuple index -> CST.Component (Region.wrap_ghost @@ ("",index))
  | Access_record str  -> CST.FieldName (Region.wrap_ghost str)
  | Access_map _ ->
    failwith @@ Format.asprintf
    "Can't decompile access_map to selection"

and decompile_lambda : dialect -> (AST.expr, AST.ty_expr) AST.lambda -> _ = fun dialect {binder;result} ->
    let var = decompile_variable @@ binder.var.wrap_content in
    let vpat : CST.var_pattern = {variable = var ; attributes = []} in
    let param_type = Option.map ~f:(prefix_colon <@ decompile_type_expr dialect) binder.ascr in
    let param_const : CST.param_const = {kwd_const=Wrap.ghost "";var=Region.wrap_ghost vpat;param_type} in
    let param_decl : CST.param_decl = ParamConst (Region.wrap_ghost param_const) in
    let param = nelist_to_npseq (param_decl, []) in
    let param : CST.parameters = Region.wrap_ghost @@ par param in
    let result,ret_type = match result.expression_content with
      AST.E_ascription {anno_expr; type_annotation} ->
      let ret_type = prefix_colon @@ (decompile_type_expr dialect) type_annotation in
      (anno_expr, Some ret_type)
    | _ -> (result,None) in
    let return = decompile_expression ~dialect result in
    (param,ret_type,return)

and decompile_declaration ~dialect : AST.declaration Location.wrap -> CST.declaration = fun decl ->
  let decl = Location.unwrap decl in
  match decl with
    Declaration_type {type_binder;type_expr; type_attr=_} ->
    let kwd_type = Wrap.ghost ""
    and name = decompile_variable type_binder
    and kwd_is = Wrap.ghost "" in
    let (params : CST.type_vars option) =
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
    let type_expr = decompile_type_expr dialect type_expr in
    let terminator = terminator dialect in
    CST.TypeDecl (Region.wrap_ghost (CST.{kwd_type; name; kwd_is; type_expr; terminator ; params}))
  | Declaration_constant {binder; attr; expr} -> (
    let attributes = decompile_attributes attr in
    let name = decompile_variable binder.var.wrap_content in
    let fun_name = name in
    let terminator = terminator dialect in
    match expr.expression_content with
      E_lambda lambda ->
      let (param,ret_type,return) = decompile_lambda dialect lambda in
      let fun_decl : CST.fun_decl = {kwd_recursive=None;kwd_function=Wrap.ghost "";fun_name;param;ret_type;kwd_is=Wrap.ghost "";return;terminator;attributes} in
      CST.FunDecl (Region.wrap_ghost fun_decl)
    | E_recursive {lambda; _} ->
      let (param,ret_type,return) = decompile_lambda dialect lambda in
      let fun_decl : CST.fun_decl = {
          kwd_recursive = Some (Wrap.ghost "");
          kwd_function=Wrap.ghost "";
          fun_name;
          param;
          ret_type;
          kwd_is=Wrap.ghost "";
          return;
          terminator;
          attributes} in
      CST.FunDecl (Region.wrap_ghost fun_decl)
    | _ ->
      let const_type = Option.map ~f:(prefix_colon <@ decompile_type_expr dialect) binder.ascr in
      let init = decompile_expression ~dialect expr in
      let vpat : CST.var_pattern = {variable =name ; attributes = []} in
      let const_decl : CST.const_decl = {kwd_const=Wrap.ghost "";pattern = PVar (Region.wrap_ghost vpat);const_type=const_type;equal=Wrap.ghost "";init;terminator; attributes} in
      CST.ConstDecl (Region.wrap_ghost const_decl)
  )
  | Declaration_module {module_binder;module_} ->
    let kwd_module = Wrap.ghost ""
    and name = Region.wrap_ghost module_binder
    and kwd_is = Wrap.ghost "" in
    let declarations = decompile_module ~dialect module_ in
    let terminator = terminator dialect in
    let enclosing = module_enclosing dialect in
    CST.ModuleDecl (Region.wrap_ghost (CST.{kwd_module; name; kwd_is; enclosing; declarations; terminator}))
  | Module_alias {alias;binders} ->
    let kwd_module = Wrap.ghost ""
    and alias   = Region.wrap_ghost alias
    and binders = nelist_to_npseq @@ List.Ne.map Region.wrap_ghost binders
    and kwd_is = Wrap.ghost "" in
    let terminator = terminator dialect in
    CST.ModuleAlias (Region.wrap_ghost (CST.{kwd_module; alias; kwd_is; binders; terminator}))


and decompile_module ?(dialect=Verbose): AST.module_ -> CST.declaration Utils.nseq = fun prg ->
  let decl = List.map ~f:(decompile_declaration ~dialect) prg in
  let decl = List.Ne.of_list decl in
  {decl;eof=Wrap.ghost ""}
