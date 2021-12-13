module List = Simple_utils.List
open Cst.Pascaligo

(* Utility functions *)

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: List.map ~f:snd tl

let npseq_to_ne_list (hd, tl) = hd, List.map ~f:snd tl

let map_npseq f (hd, tl) = f hd, List.map ~f:(fun (a,b) -> (a, f b)) tl

let fold_npseq f init (hd,tl) =
  let init = f init hd in
  List.fold ~f:(fun acc (_,b) -> f acc b) ~init tl

let pseq_to_list = function
  None -> []
| Some lst -> npseq_to_list lst

let map_pseq f = Option.map ~f:(map_npseq f)

let bind_fold_pseq f init seq =
   Option.(map ~f:(fold_npseq f init) seq |> value ~default:init)

(* FOLDING OVER THE CST *)

(* The type [folded] contains the folded functions, depending on the
   construct at hand: expressions, statements, type expressions and
   declarations. *)

type ('a, 'err) folded = {
  expr        : 'a -> expr        -> 'a;
  statement   : 'a -> statement   -> 'a;
  type_expr   : 'a -> type_expr   -> 'a;
  declaration : 'a -> declaration -> 'a
}

(* Folding over type expressions *)

let rec fold_type_expr : ('a, 'err) folded -> 'a -> type_expr -> 'a =
  fun folded init type_expr ->
  let self = fold_type_expr folded
  (* Applying the folded function to the root of the type expression *)
  and init = folded.type_expr init type_expr in
  (* Folding over the type sub-expressions *)
  match type_expr with
    T_Cart {value; region=_} ->
      let head, _, tail = value in
      let list = head :: npseq_to_list tail
      in List.fold_left ~f:self ~init list

  | T_Sum {value; region=_} ->
      let {lead_vbar=_; variants; attributes=_} = value in
      let variants = npseq_to_ne_list variants
      and aux init ({value; region=_} : _ reg) =
        let {ctor=_; args; attributes=_} = value in
        match args with
          Some (_,t) -> self init t
        | None -> init
      in List.Ne.fold_left aux init variants

  | T_Record {value; region=_} ->
      let aux init ({value; region=_} : _ reg) =
        let {field_name; field_type; attributes=_} = value
        in match field_type with
             Some (_, field_type) -> self init field_type
           | None -> self init (T_Var field_name) (* Punning *)
      in List.fold_left ~f:aux ~init @@ pseq_to_list value.elements

  | T_App {value; region=_} ->
      let _, tuple = value in
      let components = npseq_to_list tuple.value.inside
      in List.fold_left ~f:self ~init components

  | T_Fun {value; region=_} ->
      let ty1, _, ty2 = value in
      let res = self init ty1 in
      let res = self res  ty2 in
      res

  | T_Par {value; region=_} -> self init value.inside

  | T_ModPath {value; region=_} -> self init value.field

  | T_Var    _
  | T_Int    _
  | T_String _ -> init

let rec fold_expr : ('a, 'err) folded -> 'a -> expr -> 'a =
  fun f init e  ->
  let self = fold_expr f in
  let self_type = fold_type_expr f in
  let init = f.expr init e in
  let bin_op value =
    let {op=_;arg1;arg2} = value in
    let res = fold_expr f init arg1 in
    let res = fold_expr f res  arg2 in
    res
  in
  match e with
    E_Case {value;region=_} ->
      let {kwd_case=_;expr;kwd_of=_;lead_vbar=_;cases} = value in
      let res = self init expr in
      let res = matching_cases self res cases
      in res

  | E_Cond {value;region=_} ->
     let {kwd_if=_; test; kwd_then=_; kwd_then=_; if_so; if_not}
         : (expr, expr) conditional = value in
     let res = self init test in
     let res = self res if_so in
     let res = match if_not with
                 None -> res
               | Some (_, if_not) -> self res if_not
     in res

  | E_Typed {value;region=_} ->
     let (expr, (_, type_expr)) = value.inside in
     let res = self init expr in
     let res = self_type res type_expr
     in res

  | E_Or  {value;region=_} -> bin_op value
  | E_And {value;region=_} -> bin_op value
  | E_Not {value;region=_} ->
     let {op=_;arg} = value in
     let res = fold_expr f init arg
     in res

  | E_Lt    {value;region=_}
  | E_Leq   {value;region=_}
  | E_Gt    {value;region=_}
  | E_Geq   {value;region=_}
  | E_Equal {value;region=_}
  | E_Neq   {value;region=_} ->
     bin_op value

  | E_Add   {value;region=_}
  | E_Sub   {value;region=_}
  | E_Mult  {value;region=_}
  | E_Div   {value;region=_}
  | E_Mod   {value;region=_} ->
     bin_op value

  | E_Neg   {value;region=_} ->
     let {op=_;arg} = value in
     let res = fold_expr f init arg
     in res

  | E_Int   _
  | E_Nat   _
  | E_Mutez _ -> init

  | E_Cat {value;region=_} -> bin_op value

  | E_String _
  | E_Verbatim _ -> init

  | E_Cons {value;region=_} -> bin_op value

  | E_List {value;region=_} ->
     List.fold ~f:self ~init @@ pseq_to_list value.elements
  | E_Nil _ -> init

  | E_App {value;region=_} ->
     let _, expr = value in
     (match expr with
        None -> init
      | Some e ->
         List.Ne.fold_left self init @@ npseq_to_ne_list e.value.inside)

  | E_Record {value;region=_} ->
     let aux init ({value;region=_} : _ reg) =
       let {field_name=_;assignment=_;field_expr} = value in
       let res = self init field_expr
       in res
     in List.Ne.fold_left aux init
        @@ npseq_to_ne_list value.ne_elements
  | E_Proj _ -> init

  | E_Update  {value;region=_} ->
     let aux init ({value;region=_} : _ reg) =
       let {field_path=_;assignment=_;field_expr} = value in
       let res = self init field_expr
       in res
     in List.Ne.fold_left aux init
        @@ npseq_to_ne_list value.updates.value.ne_elements
  | E_ModPath {value;region=_} -> self init value.field

  | E_Var _ -> init

  | E_Call {value;region=_} ->
    let (lam, args) = value in
    let res = self init lam in
    List.Ne.fold_left self res @@ npseq_to_ne_list args.value.inside
  | E_Bytes _ -> init

  | E_Tuple {value;region=_} ->
     List.Ne.fold_left self init @@ npseq_to_ne_list value.inside
  | E_Par {value;region=_} ->
     self init value.inside

  | E_Fun {value;region=_} ->
     let ({kwd_function=_; parameters=_; ret_type; kwd_is=_;
           return; attributes=_}: fun_expr) = value in
     let res = self init return in
     (match ret_type with
        Some (_, ty) -> self_type res ty
      | None -> res)

  | E_CodeInj {value;region=_} ->
    let {language=_;code;rbracket=_} = value in
    self init code

  | E_Set {value;region=_} ->
     List.fold ~f:self ~init @@ pseq_to_list value.elements
  | E_SetMem {value;region=_} ->
     let {set;kwd_contains=_;element} = value in
     let res = self init set in
     let res = self res element
     in res

  | E_MapLookup {value;region=_} ->
     let {map=_;index} = value in
     self init index.value.inside

  | E_Map {value;region=_}
  | E_BigMap {value;region=_} ->
     let aux init ({value;region=_}: _ reg) =
       let {key; arrow=_; value} = value in
       let res = self init key in
       let res = self res value
       in res
     in List.fold ~f:aux ~init @@ pseq_to_list value.elements
  | E_Block {value;region=_} ->
     let {block=b;kwd_with=_;expr} = value in
     let res = fold_block f init b in
     let res = self res expr
     in res

  | E_Ctor _ -> failwith "TODO" (* TODO *)


and fold_block f init ({value; region=_}: block reg) =
  let {enclosing=_; statements; terminator=_} = value in
  let res =
    List.Ne.fold_left(fold_statement f) init
    @@ npseq_to_ne_list statements
  in res

and fold_statement : ('a, 'err) folded -> 'a -> statement -> 'a = fun f init s  ->
  let self = fold_statement f in
  let self_expr = fold_expr f in
  let self_type = fold_type_expr f in
  let self_module = fold_module f in
  let init = f.statement init s in
  let if_clause res = function
    ClauseInstr inst -> self res @@ S_Instr inst
  | ClauseBlock block -> fold_block f res block
 (* | ClauseBlock ShortBlock {value;region=_} -> List.Ne.fold_left self res @@ npseq_to_ne_list @@ fst value.inside *)
  in
  let fold_selection init = function
    FieldName _ -> init
  | Component _ -> init
  in
  let fold_path init = function
    Name _ -> init
  | Path {value;region=_} ->
    let {struct_name=_;selector=_;field_path} = value in
    List.Ne.fold_left fold_selection init @@ npseq_to_ne_list field_path
  in
  match s with
    S_Instr I_Cond {value;region=_} ->
    let {kwd_if=_; test; kwd_then=_; if_so; if_not} :
          (test_clause, test_clause) conditional = value in
    let res = self_expr init test in
    let res = if_clause res if_so in
    let res = match if_not with
                None -> res
              | Some (_, if_not) -> if_clause res if_not
    in res

  | S_Instr I_Case {value; region=_} ->
     let {kwd_case=_; expr; kwd_of=_;
          enclosing=_; lead_vbar=_; cases} = value in
     let res = self_expr init expr in
     let res = matching_cases if_clause res cases
     in res

  | S_Instr I_Assign {value;region=_} ->
     let {lhs; assign=_;rhs} = value in
     let fold_lhs res (lhs:lhs) = match lhs with
       Path path -> fold_path res path
     | MapPath {value;region=_} ->
        let {path;index} = value in
        let res = fold_path res path in
        let res = self_expr res index.value.inside
        in res in
     let res = fold_lhs init lhs in
     let res = self_expr res rhs
     in res

  | S_Instr I_While {value; region=_} ->
     let {kwd_while=_; cond; block} = value in
     let res = self_expr init cond in
     let res = fold_block f res block
     in res

  | S_Instr I_For {value; region=_} ->
     let {kwd_for=_; index=_; assign=_; init=i;
          kwd_to=_; bound; step; block} = value in
     let res = self_expr init i in
     let res = self_expr res bound in
     let res = match step with
                 Some (_, expr) -> self_expr res expr
               | None -> res in
     let res = fold_block f res block
     in res

  | S_Instr I_ForIn {value;region=_} ->
     let {kwd_for=_; var=_; bind_to=_; kwd_in=_;
          kind=_; collection=expr; block} = value in
     let res = self_expr init expr in
     let res = fold_block f res block
     in res

  | S_Instr I_Call {value;region=_} ->
    let (expr, arguments) = value in
    let res = self_expr init expr in
    let res = List.Ne.fold_left self_expr res
              @@ npseq_to_ne_list arguments.value.inside
    in res

  | S_Instr I_Skip _ -> init

  | S_Instr I_Patch {value;region=_} ->
    let {kwd_patch=_; collection; kwd_with=_; patch} = value in
    let res = fold_path init collection in
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let res = self_expr init field_expr in
      res
    in
    let res = List.Ne.fold_left aux res @@ npseq_to_ne_list record_inj.value.ne_elements in
    res

(* TODO: Ã fusionner avec le cas ci-dessus

  | Instr MapPatch {value;region=_} ->
    let {kwd_patch=_;path;kwd_with=_;map_inj} = value in
    let res = fold_path init path in
    let aux init ({value;region=_} : _ reg) =
      let {source;arrow=_;image} = value in
      let res = self_expr init source in
      let res = self_expr res  image in
      res in
    let res = List.Ne.fold_left aux res @@ npseq_to_ne_list map_inj.value.ne_elements
    in res
  | Instr SetPatch {value;region=_} ->
    let {kwd_patch=_;path;kwd_with=_;set_inj} = value in
    let res = fold_path init path in
    let res = List.Ne.fold_left self_expr res @@ npseq_to_ne_list set_inj.value.ne_elements
    in res
 *)

  | S_Instr I_Remove {value;region=_} ->
    let {kwd_remove=_; item; kwd_from=_; collection} = value in
    let res = self_expr init item in
    let res = fold_path res collection
    in res

  (* TODO À fusionner avec le cas ci-dessus

| Instr SetRemove   {value;region=_} ->
    let {kwd_remove=_;element;kwd_from=_;kwd_set=_;set} = value in
    let res = self_expr init element in
    let res = fold_path res set in
    res
 *)

  | S_Decl D_Const {value;region=_} ->
     let {kwd_const=_; pattern=_; const_type; equal=_; init=expr;
          terminator=_; attributes=_} = value in
     let res = self_expr init expr in
     (match const_type with
        Some (_, ty) -> self_type res ty
      | None -> res)

  | S_VarDecl {value;region=_} ->
     let {kwd_var=_; pattern=_; var_type; assign=_; init=expr;
          terminator=_; attributes=_} = value in
    let res = self_expr init expr in
    (match var_type with
      Some (_, ty) -> self_type res ty
    | None ->res)

  | S_Decl D_Fun {value;region=_} ->
     let {kwd_recursive=_; kwd_function=_; fun_name=_; parameters=_;
          ret_type; kwd_is=_; return; terminator=_; attributes=_} = value in
     let res = self_expr init return in
     (match ret_type with
        Some (_, ty) -> self_type res ty
      | None -> res)

  | S_Decl D_Type {value;region=_} ->
     let {kwd_type=_; name=_; params=_; kwd_is=_;
          type_expr; terminator=_} = value in
    let res = self_type init type_expr in
    res

  | S_Decl D_Module {value;region=_} ->
    let {kwd_module=_;name=_;kwd_is=_;enclosing=_;declarations;terminator=_} = value in
    let res = self_module init declarations in
    res

  | S_Decl D_ModAlias {value;region=_} ->
    let {kwd_module=_;alias=_;kwd_is=_;binders=_;terminator=_} = value in
    init

  | S_Decl D_Directive _ -> failwith "TODO" (* TODO *)


(* TODO: Cases missing *)

and matching_cases : type b.('a -> b -> _) -> 'a -> (b case_clause reg, _) Utils.nsepseq -> _ =
  fun self init value ->
  let case_clause self init ({value;region=_}: _ case_clause reg) =
    let {pattern=_;arrow=_;rhs} = value in
    self init rhs
  in List.Ne.fold_left(case_clause self) init
     @@ npseq_to_ne_list value

and fold_declaration : ('a, 'err) folded -> 'a -> declaration -> 'a =
  fun f init d ->
  let self_expr = fold_expr f in
  let self_type = fold_type_expr f in
  let self_module = fold_module f in
  let init = f.declaration init d in
  match d with
    D_Const {value;region=_} ->
    let {kwd_const=_;pattern=_;const_type;equal=_;init=expr;terminator=_;attributes=_} = value in
    let res = self_expr init expr in
    (match const_type with
      Some (_, ty) -> self_type res ty
    | None ->    res
    )
  | D_Fun {value;region=_} ->
    let {kwd_recursive=_;kwd_function=_;fun_name=_;parameters=_;ret_type;kwd_is=_;return;terminator=_;attributes=_} = value in
    let res = self_expr init return in
    (match ret_type with
      Some (_, ty) -> self_type res ty
    | None ->    res
    )
  | D_Type {value;region=_} ->
    let {kwd_type=_;name=_;kwd_is=_;type_expr;terminator=_;params=_} = value in
    let res = self_type init type_expr in
    res
  | D_Module {value;region=_} ->
    let {kwd_module=_;name=_;kwd_is=_;enclosing=_;module_;terminator=_} = value in
    let res = self_module init module_ in
    res
  | D_ModAlias {value;region=_} ->
    let {kwd_module=_;alias=_;kwd_is=_;binders=_;terminator=_} = value in
    init
  | D_Directive _ -> init

and fold_module : ('a, 'err) folded -> 'a -> t -> 'a =
  fun f init {decl;eof=_} ->
  let self = fold_declaration f in
  List.Ne.fold_left self init @@ decl

type 'err mapped = {
  expr        : expr -> expr;
  type_expr   : type_expr -> type_expr;
  statement   : statement -> statement;
  declaration : declaration -> declaration
}

let rec map_type_expression : 'err mapped -> type_expr -> 'b =
  fun f t ->
  let self = map_type_expression f in
  let t = f.type_expr t in
  let return a = a in
  match t with
    T_Cart {value;region} ->
    let value = map_npseq self value in
    return @@ T_Cart {value; region}
  | T_Sum {value;region} ->
    let aux (e : variant reg) =
      let arg = Option.map ~f:(fun (a,b) -> let b = self b in (a,b)) e.value.arg in
      let value = {e.value with arg} in
      {e with value} in
    let variants = map_npseq aux value.variants in
    let value = {value with variants} in
    return @@ T_Sum {value; region}
  | T_Record {value;region} ->
    let aux (element : _ reg ) =
      let field_type = self element.value.field_type in
      let value = {element.value with field_type} in
      {element with value} in
    let ne_elements = map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ T_Record {value; region}
  | T_App {value;region} ->
    let (const, tuple) = value in
    let inside = map_npseq self tuple.value.inside in
    let tuple = {tuple with value = {tuple.value with inside}} in
    let value = (const, tuple) in
    return @@ T_App {value; region}
  | T_Fun {value; region} ->
    let (ty1, wild, ty2) = value in
    let ty1 = self ty1 in
    let ty2 = self ty2 in
    let value = (ty1, wild, ty2) in
    return @@ T_Fun {value;region}
  | T_Par {value; region} ->
    let inside = self value.inside in
    let value = {value with inside} in
    return @@ T_Par {value;region}
  | T_ModPath {value;region} ->
    let field = self value.field in
    let value = {value with field} in
    return @@ T_ModPath {value;region}
  | T_Var    _
  | T_Int    _
  | T_String _ as e -> e

let rec map_expression : 'err mapped -> expr -> expr =
  fun f e  ->
  let self = map_expression f in
  let self_type = map_type_expression f in
  let return a = a in
  let e = f.expr e in
  let bin_op value =
    let {op;arg1;arg2} = value in
    let arg1 = self arg1 in
    let arg2 = self arg2 in
    {op;arg1;arg2}
  in
  match e with
    E_Case {value;region} ->
      let {kwd_case=_; expr; kwd_of=_; enclosing=_;
           lead_vbar=_; cases} = value in
      let expr = self expr in
      let cases = matching_cases self cases in
      let value = {value with expr; cases} in
      return @@ E_Case {value;region}
  | E_Cond {value;region} ->
     let ({kwd_if=_; test; kwd_then=_; if_so; if_not}
          : expr conditional) = value in
     let test = self test in
     let if_so = self if_so in
     let if_not =
       match if_not with
         None -> if_not
       | Some (kwd_else, if_not) ->
          Some (kwd_else, self if_not) in
     let value = {value with test; if_so; if_not}
     in return @@ E_Cond {value;region}
  | E_Typed {value;region} ->
     let expr, (comma, type_expr) = value.inside in
     let expr = self expr in
     let type_expr = self_type type_expr in
     let inside = expr, (comma, type_expr) in
     let value = {value with inside} in
     return @@ E_Typed {value;region}
  | E_Or {value;region} ->
     let value = bin_op value in
     return @@ E_Or {value;region}
  | E_And {value;region} ->
     let value = bin_op value in
     return @@ E_And {value;region}
  | E_Not {value;region} ->
     let arg = self value.arg in
     let value = {value with arg} in
     return @@ E_Not {value;region}
  | E_Lt {value; region} ->
     let value = bin_op value in
     return @@ E_Lt {value;region}
  | E_Leq {value;region} ->
     let value = bin_op value in
     return @@ E_Leq {value;region}
  | E_Gt {value;region} ->
     let value = bin_op value in
     return @@ E_Gt {value;region}
  | E_Geq {value;region} ->
     let value = bin_op value in
     return @@ E_Geq {value;region}
  | E_Equal {value;region} ->
     let value = bin_op value in
     return @@ E_Equal {value;region}
  | E_Neq {value;region} ->
     let value = bin_op value in
     return @@ E_Neq {value;region}
  | E_Add {value;region} ->
     let value = bin_op value in
     return @@ E_Add {value;region}
  | E_Sub {value;region} ->
     let value = bin_op value in
     return @@ E_Sub {value;region}
  | E_Mult {value;region} ->
     let value = bin_op value in
     return @@ E_Mult {value;region}
  | E_Div {value;region} ->
     let value = bin_op value in
     return @@ E_Div {value;region}
  | E_Mod {value;region} ->
     let value = bin_op value in
     return @@ E_Mod {value;region}
  | E_Neg {value; region} ->
     let arg = self value.arg in
     let value = {value with arg} in
     return @@ E_Neg {value; region}

  | E_Int   _
  | E_Nat   _
  | E_Mutez _ as e -> return e

  | E_Cat {value;region} ->
     let value = bin_op value in
     return @@ E_Cat {value;region}

  | E_String _
  | E_Verbatim _ as e -> return e

  | E_Cons {value; region} ->
     let value = bin_op value in
     return @@ E_Cons {value; region}
  | E_List {value;region} ->
     let elements = map_pseq self value.elements in
     let value = {value with elements} in
     return @@ E_List {value;region}
  | E_Nil _ as e -> return e

  | E_App {value;region} ->
     let const, expr = value in
     let expr = Option.map
                  ~f:(fun (e : tuple_expr) ->
                      let inside = map_npseq self e.value.inside in
                      {e with value = {e.value with inside}})
                  expr in
     let value = const, expr in
     return @@ E_App {value;region}
  | E_Record {value; region} ->
     let aux (e : field_assignment reg) =
       let field_expr = self e.value.field_expr in
       {e with value = {e.value with field_expr}} in
     let ne_elements = map_npseq aux value.ne_elements in
     let value = {value with ne_elements} in
     return @@ E_Record {value; region}
  | E_Proj _  as e -> return e
  | E_Update {value;region} ->
     let aux (e : field_path_assignment reg) =
       let field_expr = self e.value.field_expr in
       {e with value = {e.value with field_expr}} in
     let ne_elements = map_npseq aux value.updates.value.ne_elements in
     let updates =
       {value.updates with value = {value.updates.value with ne_elements}} in
     let value = {value with updates} in
     return @@ E_Update {value;region}
  | E_ModPath {value; region} ->
     let field = self value.field in
     let value = {value with field} in
     return @@ E_ModPath {value; region}
  | E_Var _ as e -> return e

  | E_Call {value; region} ->
     let lambda, args = value in
     let lambda = self lambda in
     let inside = map_npseq self args.value.inside in
     let args = {args with value = {args.value with inside}} in
     let value = lambda, args in
     return @@ E_Call {value; region}
  | E_Bytes _ as e -> return e

  | E_Tuple {value; region} ->
     let inside = map_npseq self value.inside in
     let value = {value with inside} in
     return @@ E_Tuple {value; region}
  | E_Par {value; region} ->
     let inside = self value.inside in
     let value = {value with inside} in
     return @@ E_Par {value; region}
  | E_Fun {value;region} ->
     let ({kwd_function=_; parameters=_; ret_type; kwd_is=_;
           return=body; attributes=_} : fun_expr) = value in
     let body = self body in
     let ret_type =
       Option.map ~f:(fun (a,b) -> let b = self_type b in (a,b)) ret_type in
     let value = {value with return=body; ret_type}
     in return @@ E_Fun {value;region}
  | E_CodeInj {value; region} ->
     let code = self value.code in
     let value = {value with code} in
     return @@ E_CodeInj {value; region}
  | E_Set {value;region} ->
     let elements = map_pseq self value.elements in
     let value = {value with elements} in
     return @@ E_Set {value;region}
  | E_SetMem {value; region} ->
     let {set; kwd_contains; element} = value in
     let set = self set in
     let element = self element in
     let value = {set; kwd_contains; element}
     in return @@ E_SetMem {value; region}
  | E_MapLookup {value;region} ->
     let {map; index} = value in
     let inside = self index.value.inside in
     let index = {index with value = {index.value with inside}} in
     let value = {map; index} in
     return @@ E_MapLookup {value; region}
  | E_Map {value; region} ->
     let aux (b: binding reg) =
       let {key; arrow; value} = b.value in
       let key = self key in
       let value  = self value in
       let value = {key; arrow; value}
       in {b with value} in
     let elements = map_pseq aux value.elements in
     let value = {value with elements} in
     return @@ E_Map {value;region}
  | E_BigMap {value; region} ->
     let aux (b: binding reg) =
       let {key; arrow; value} = b.value in
       let key = self key in
       let value  = self value in
       let value = {key; arrow; value}
       in {b with value} in
     let elements = map_pseq aux value.elements in
     let value = {value with elements} in
     return @@ E_BigMap {value; region}
  | E_Block {value;region} ->
     let {block; kwd_with; expr} = value in
     let expr = self expr in
     let block = map_block f block in
     let value = {block; kwd_with; expr}
     in return @@ E_Block {value;region}
  | E_Ctor _ -> failwith "TODO" (* TODO *)

and map_block f (block: block reg) =
  let {enclosing=_; statements; terminator=_} = block.value in
  let statements = map_npseq (map_statement f) statements in
  let value = {block.value with statements}
  in {block with value}

and map_statement : 'err mapped -> statement -> statement = fun f s  ->
  let self_expr = map_expression f in
  let self_inst = map_instruction f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let s = f.statement s in
  match s with
  | S_Instr inst ->
      let inst = self_inst inst in S_Instr inst

  | S_Decl D_Const {value;region} ->
     let {kwd_const=_; pattern=_; const_type; equal=_; init; terminator=_;
          attributes=_} = value in
     let init = self_expr init in
     let const_type =
       Option.map ~f:(fun (w, ty) -> let ty = self_type ty in (w,ty)) const_type in
     let value = {value with init; const_type}
     in S_Decl (D_Const {value; region})

  | S_VarDecl {value;region} ->
     let {kwd_var=_; pattern=_; var_type; assign=_; init; terminator=_} = value in
     let var_type =
       Option.map ~f:(fun (w, ty) -> let ty = self_type ty in (w,ty)) var_type in
     let value = {value with init; var_type}
     in S_VarDecl {value; region}

  | S_Decl D_Fun {value;region} ->
     let {kwd_recursive=_; kwd_function=_; fun_name=_; parameters=_;
          ret_type; kwd_is=_; return; terminator=_; attributes=_} = value in
     let return = self_expr return in
     let ret_type =
       Option.map ~f:(fun (w, ty) -> let ty = self_type ty in (w,ty)) ret_type in
     let value = {value with return;ret_type}
     in S_Decl (D_Fun {value;region})

  | S_Decl D_Type {value;region} ->
     let {kwd_type=_; name=_; kwd_is=_; type_expr; terminator=_} = value in
     let type_expr = self_type type_expr in
     let value = {value with type_expr}
     in S_Decl (D_Type {value;region})

  | S_Decl D_Module {value;region} ->
     let {kwd_module=_; name=_; kwd_is=_;
          enclosing=_; declarations; terminator=_} = value in
     let declarations = self_module declarations in
     let value = {value with declarations}
     in S_Decl (D_Module {value;region})

  | S_Decl D_ModAlias {value;region} ->
     S_Decl (D_ModAlias {value;region}) (* TODO? *)

  | S_Decl D_Directive _ -> failwith "TODO" (* TODO *)


and map_instruction f i =
  let self = map_instruction f in
  let self_stat = map_statement f in
  let self_expr = map_expression f in
  let if_clause = function
      ClauseInstr instr -> ClauseInstr (self instr)
    | ClauseBlock block -> ClauseBlock (map_block f block)
(*
    | ClauseBlock ShortBlock {value;region} ->
      let (s,s_opt) = value.inside in
      let s = map_npseq self_stat @@ s in
      let value = {value with inside = (s,s_opt)} in
      ClauseBlock (ShortBlock {value;region}) *)
  in
  let map_selection = function
    FieldName _
  | Component _ as s -> s
  in
  let map_path = function
    Name _ as n -> n
  | Path {value;region} ->
    let {struct_name=_;selector=_;field_path} = value in
    let field_path = map_npseq map_selection field_path in
    let value = {value with field_path} in
    (Path {value;region} : path)
  in
  match i with
    I_Cond {value; region} ->
      let {kwd_if; test; kwd_then; if_so; if_not}
          : (test_clause, test_clause) conditional = value in
      let test  = self_expr test in
      let if_so  = if_clause if_so in
      let if_not =
        match if_not with
          None -> if_not
        | Some (kwd_else, if_not) ->
           Some (kwd_else, if_clause if_not) in
      let value : (test_clause, test_clause) conditional =
        {kwd_if; test; kwd_then; if_so; if_not}
      in I_Cond {value; region}

  | I_Case {value; region} ->
     let {kwd_case=_; expr; kwd_of=_;
          enclosing=_; lead_vbar=_; cases} = value in
     let expr = self_expr expr in
     let cases = matching_cases if_clause cases in
     let value = {value with expr;cases}
     in I_Case {value; region}

  | I_Assign {value; region} ->
     let {lhs; assign;rhs} = value in
     let map_lhs (lhs:lhs) : lhs = match lhs with
         Path path -> let path = map_path path in (Path path : lhs)
       | MapPath {value;region} ->
          let {path;index} = value in
          let path = map_path path in
          let inside = self_expr index.value.inside in
          let value = {path;
                       index={index with value = {index.value with inside}}} in
          MapPath {value;region} in
     let lhs = map_lhs lhs in
     let rhs = self_expr rhs in
     let value = {lhs;assign;rhs}
     in I_Assign {value;region}

  | I_While {value;region} ->
    let {kwd_while; cond; block} = value in
    let cond = self_expr cond in
    let block = map_block f block in
    let value = {kwd_while; cond; block}
    in I_While {value; region}

  | I_For {value; region} ->
     let {kwd_for=_; index=_; assign=_; init; kwd_to=_;
          bound; step; block} = value in
    let init = self_expr init in
    let bound = self_expr bound in
    let step =
      Option.map ~f:(fun (w,s) -> let s = self_expr s in (w,s)) step in
    let block = map_block f block in
    let value = {value with init; bound; step; block}
    in I_For {value;region}

  | I_ForIn {value; region} ->
     let {kwd_for=_; var=_; bind_to=_; kwd_in=_;
          collection; block} = value in
    let collection = self_expr collection in
    let block = map_block f block in
    let value = {value with collection; block}
    in I_ForIn {value; region}

  | I_Call {value; region} ->
    let expr, arguments = value in
    let expr = self_expr expr in
    let inside = map_npseq self_expr arguments.value.inside in
    let arguments = {arguments with value = {arguments.value with inside}} in
    let value = expr, arguments
    in I_Call {value; region}

  | I_Skip _ as i -> i

  | I_Patch {value;region} -> (* Was RecordPatch *)
    let {kwd_patch=_;path;kwd_with=_;record_inj} = value in
    let path = map_path path in
    let aux ({value;region} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let field_expr = self_expr field_expr in
      let value = {value with field_expr} in
      ({value;region} : _ reg)
    in
    let ne_elements = map_npseq  aux @@ record_inj.value.ne_elements in
    let record_inj = {record_inj with value = {record_inj.value with ne_elements}} in
    let value = {value with path;record_inj}
    in RecordPatch {value;region}

  (* TODO : Fusionner avec le cas ci-dessus

  | MapPatch    {value;region} ->
    let {kwd_patch=_;path;kwd_with=_;map_inj} = value in
    let path = map_path path in
    let aux ({value;region} : _ reg) =
      let {source;arrow;image} = value in
      let source = self_expr source in
      let image = self_expr image in
      let value = {source;arrow;image} in
      ({value;region} : _ reg)
    in
    let ne_elements = map_npseq aux @@ map_inj.value.ne_elements in
    let map_inj = {map_inj with value = {map_inj.value with ne_elements}} in
    let value = {value with path;map_inj} in
    MapPatch {value;region}
  | SetPatch    {value;region} ->
    let {kwd_patch=_;path;kwd_with=_;set_inj} = value in
    let path = map_path path in
    let ne_elements = map_npseq self_expr @@ set_inj.value.ne_elements in
    let set_inj = {set_inj with value = {set_inj.value with ne_elements}} in
    let value = {value with path;set_inj} in
    SetPatch {value;region}
 *)

  | I_Remove _ -> failwith "TODO"

(* TODO: Fusionner les deux cas suivants
  | MapRemove   {value;region} ->
    let {kwd_remove=_;key;kwd_from=_;kwd_map=_;map} = value in
    let key = self_expr key in
    let map = map_path map in
    let value = {value with key;map} in
    MapRemove {value;region}
  | SetRemove   {value;region} ->
    let {kwd_remove=_;element;kwd_from=_;kwd_set=_;set} = value in
    let element = self_expr element in
    let set = map_path set in
    let value = {value with element;set} in
    SetRemove {value;region}
 *)

and matching_cases :
type b. (b-> b) -> (b case_clause reg,_) Utils.nsepseq reg -> (b case_clause reg,_) Utils.nsepseq =
  fun self cases ->
  let case_clause self (case_clause: _ case_clause reg) =
    let {pattern=_;arrow=_;rhs} = case_clause.value in
    let rhs = self rhs in
    let value = {case_clause.value with rhs} in
    {case_clause with value}
  in map_npseq (case_clause self) cases.value

 and map_declaration : 'err mapped -> declaration -> declaration =
  fun f d ->
  let self_expr = map_expression f in
  let self_type = map_type_expression f in
  let self_module = map_module f in
  let return a = a in
  let d = f.declaration d in
  match d with
    D_Const {value; region} ->
      let {kwd_const=_; pattern=_; const_type; equal=_;
           init; terminator=_; attributes=_} = value in
      let init = self_expr init in
      let const_type =
        Option.map ~f:(fun (a,b) -> let b = self_type b in (a,b)) const_type in
      let value = {value with init; const_type}
      in return @@ D_Const {value; region}
  | D_Fun {value; region} ->
     let {kwd_recursive=_; kwd_function=_; fun_name=_; parameters=_;
          ret_type; kwd_is=_; return=expr; terminator=_; attributes=_} = value in
     let expr = self_expr expr in
     let ret_type =
       Option.map ~f:(fun (a,b) -> let b = self_type b in (a,b)) ret_type in
     let value = {value with return = expr; ret_type} in
     return @@ D_Fun {value;region}
  | D_Type {value; region} ->
      let {kwd_type=_; name=_; params=_; kwd_is=_;
           type_expr; terminator=_} = value in
      let type_expr = self_type type_expr in
      let value = {value with type_expr} in
      return @@ D_Type {value;region}
  | D_Module {value;region} ->
     let {kwd_module=_; name=_; kwd_is=_; enclosing=_;
          declarations; terminator=_} = value in
     let declarations = self_module declarations in
     let value = {value with declarations}
     in return @@ D_Module {value;region}
  | D_ModAlias {value; region} ->
     let {kwd_module=_; alias=_; kwd_is=_;
          mod_path=_; terminator=_} = value in
     return @@ D_ModAlias {value;region}
  | D_Directive _ as d -> return d

and map_module : 'err mapped -> t -> t =
  fun f {decl;eof} ->
  let self = map_declaration f in
  (fun decl -> {decl; eof}) @@
  List.Ne.map self @@ decl

(* TODO this is stupid *)
(*
let fold_to_map : unit -> (unit, 'err) folded -> 'err mapped =
  fun init {expr; type_expr; statement; declaration} ->
  let expr        e = expr        init e; e
  and type_expr   t = type_expr   init t; t
  and statement   s = statement   init s; s
  and declaration d = declaration init d; d
  in {expr; type_expr; statement; declaration}
 *)
