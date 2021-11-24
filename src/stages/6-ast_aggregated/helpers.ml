open Types

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map ~f:(fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all ~f:(fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let tuple_of_record (m: _ LMap.t) =
  let aux i = 
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind ~f: (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

let rec subst_type v t (u : type_expression) =
  let self = subst_type in
  match u.type_content with
  | T_variable v' when Var.equal v v' -> t
  | T_arrow {type1;type2} ->
     let type1 = self v t type1 in
     let type2 = self v t type2 in
     { u with type_content = T_arrow {type1;type2} }
  | T_abstraction {ty_binder;kind;type_} when not (Var.equal (Location.unwrap ty_binder) v) ->
     let type_ = self v t type_ in
     { u with type_content = T_abstraction {ty_binder;kind;type_} }
  | T_for_all {ty_binder;kind;type_} when not (Var.equal (Location.unwrap ty_binder) v) ->
     let type_ = self v t type_ in
     { u with type_content = T_for_all {ty_binder;kind;type_} }
  | T_constant {language;injection;parameters} ->
     let parameters = List.map ~f:(self v t) parameters in
     { u with type_content = T_constant {language;injection;parameters} }
  | T_sum {content; layout} ->
     let content = LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self v t associated_type; michelson_annotation;decl_pos}) content in
     { u with type_content = T_sum {content; layout} }
  | T_record {content; layout} ->
     let content = LMap.map (fun {associated_type; michelson_annotation; decl_pos} : row_element ->
                       {associated_type = self v t associated_type; michelson_annotation;decl_pos}) content in
     { u with type_content = T_record {content; layout} }
  | _ -> u

(* This function transforms a type `∀ v1 ... vn . t` into the pair `([ v1 ; .. ; vn ] , t)` *)
let destruct_for_alls (t : type_expression) =
  let rec destruct_for_alls type_vars (t : type_expression) = match t.type_content with
    | T_for_all { ty_binder ; type_ ; _ } ->
       destruct_for_alls (Location.unwrap ty_binder :: type_vars) type_
    | _ -> (type_vars, t)
  in destruct_for_alls [] t

let constant_compare ia ib =
  let open Stage_common.Constant in
  let ia' = Ligo_string.extract ia in
  let ib' = Ligo_string.extract ib in
  match ia',ib' with
  | a,b when (String.equal a map_name || String.equal a map_or_big_map_name) && (String.equal b map_name || String.equal b map_or_big_map_name) -> 0
  | a,b when (String.equal a big_map_name || String.equal a map_or_big_map_name) && (String.equal b big_map_name || String.equal b map_or_big_map_name) -> 0
  | _ -> Ligo_string.compare ia ib

let assert_eq = fun a b -> if (a = b) then Some () else None
let assert_same_size = fun a b -> if (List.length a = List.length b) then Some () else None

let rec assert_type_expression_eq (a, b: (type_expression * type_expression)) : unit option =
  let open Option in
  match (a.type_content, b.type_content) with
  | T_constant {language=la;injection=ia;parameters=lsta}, T_constant {language=lb;injection=ib;parameters=lstb} -> (
    if (String.equal la lb) && (constant_compare ia ib = 0) then (
      let* _ = assert_same_size lsta lstb in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> assert_type_expression_eq p) ~init:(Some ()) (List.zip_exn lsta lstb)
    ) else
      None
  )
  | T_constant _, _ -> None
  | T_sum sa, T_sum sb -> (
      let sa' = LMap.to_kv_list_rev sa.content in
      let sb' = LMap.to_kv_list_rev sb.content in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let* _ = assert_eq ka kb in
        assert_type_expression_eq (va, vb)
      in
      let* _ = assert_same_size sa' sb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn sa' sb')
    )
  | T_sum _, _ -> None
  | T_record ra, T_record rb
       when is_tuple_lmap ra.content <> is_tuple_lmap rb.content -> None
  | T_record ra, T_record rb -> (
      let sort_lmap r' = List.sort ~compare:(fun (Label a,_) (Label b,_) -> String.compare a b) r' in
      let ra' = sort_lmap @@ LMap.to_kv_list_rev ra.content in
      let rb' = sort_lmap @@ LMap.to_kv_list_rev rb.content in
      let aux ((ka, {associated_type=va;_}), (kb, {associated_type=vb;_})) =
        let Label ka = ka in
        let Label kb = kb in
        let* _ = assert_eq ka kb in
        assert_type_expression_eq (va, vb)
      in
      let* _ = assert_eq ra.layout rb.layout in
      let* _ = assert_same_size ra' rb' in
      List.fold_left ~f:(fun acc p -> match acc with | None -> None | Some () -> aux p) ~init:(Some ()) (List.zip_exn ra' rb')

    )
  | T_record _, _ -> None
  | T_arrow {type1;type2}, T_arrow {type1=type1';type2=type2'} ->
    let* _ = assert_type_expression_eq (type1, type1') in
    assert_type_expression_eq (type2, type2')
  | T_arrow _, _ -> None
  | T_variable x, T_variable y ->
     (* TODO : we must check that the two types were bound at the same location (even if they have the same name), i.e. use something like De Bruijn indices or a propper graph encoding *)
     if Var.equal x y then Some () else None
  | T_variable _, _ -> None
  | T_singleton a , T_singleton b -> assert_literal_eq (a , b)
  | T_singleton _ , _ -> None
  | T_abstraction a , T_abstraction b ->
    assert_type_expression_eq (a.type_, b.type_) >>= fun _ ->
    Some (assert (equal_kind a.kind b.kind))
  | T_for_all a , T_for_all b ->
    assert_type_expression_eq (a.type_, b.type_) >>= fun _ ->
    Some (assert (equal_kind a.kind b.kind))
  | T_abstraction _ , _ -> None
  | T_for_all _ , _ -> None

and type_expression_eq ab = Option.is_some @@ assert_type_expression_eq ab

and assert_literal_eq (a, b : literal * literal) : unit option =
  match (a, b) with
  | Literal_int a, Literal_int b when a = b -> Some ()
  | Literal_int _, Literal_int _ -> None
  | Literal_int _, _ -> None
  | Literal_nat a, Literal_nat b when a = b -> Some ()
  | Literal_nat _, Literal_nat _ -> None
  | Literal_nat _, _ -> None
  | Literal_timestamp a, Literal_timestamp b when a = b -> Some ()
  | Literal_timestamp _, Literal_timestamp _ -> None
  | Literal_timestamp _, _ -> None
  | Literal_mutez a, Literal_mutez b when a = b -> Some ()
  | Literal_mutez _, Literal_mutez _ -> None
  | Literal_mutez _, _ -> None
  | Literal_string a, Literal_string b when a = b -> Some ()
  | Literal_string _, Literal_string _ -> None
  | Literal_string _, _ -> None
  | Literal_bytes a, Literal_bytes b when a = b -> Some ()
  | Literal_bytes _, Literal_bytes _ -> None
  | Literal_bytes _, _ -> None
  | Literal_unit, Literal_unit -> Some ()
  | Literal_unit, _ -> None
  | Literal_address a, Literal_address b when a = b -> Some ()
  | Literal_address _, Literal_address _ -> None
  | Literal_address _, _ -> None
  | Literal_signature a, Literal_signature b when a = b -> Some ()
  | Literal_signature _, Literal_signature _ -> None
  | Literal_signature _, _ -> None
  | Literal_key a, Literal_key b when a = b -> Some ()
  | Literal_key _, Literal_key _ -> None
  | Literal_key _, _ -> None
  | Literal_key_hash a, Literal_key_hash b when a = b -> Some ()
  | Literal_key_hash _, Literal_key_hash _ -> None
  | Literal_key_hash _, _ -> None
  | Literal_chain_id a, Literal_chain_id b when a = b -> Some ()
  | Literal_chain_id _, Literal_chain_id _ -> None
  | Literal_chain_id _, _ -> None
  | Literal_operation _, Literal_operation _ -> None
  | Literal_operation _, _ -> None
