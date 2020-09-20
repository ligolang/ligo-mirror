module AST = Ast_typed
open AST
open Errors
module Append_tree = Tree.Append
open! Mini_c
open Trace

let t_record_to_pairs ?(layout = L_tree) return compile_type m =
  let open AST.Helpers in
  let is_tuple_lmap = is_tuple_lmap m in
  let lst = kv_list_of_t_record_or_tuple ~layout m in
  match layout with
  | L_tree -> (
      let node = Append_tree.of_list lst in
      let aux a b : (type_expression annotated, spilling_error) result =
        let%bind a = a in
        let%bind b = b in
        let%bind t = return @@ T_pair (a, b) in
        ok (None, t)
      in
      let%bind m' = Append_tree.fold_ne
          (fun (Label ann, ({associated_type;_}: AST.row_element)) ->
             let%bind a = compile_type associated_type in
             ok ((if is_tuple_lmap then 
                    None 
                  else 
                    Some ann), 
                 a)
          )
          aux node in
      ok @@ snd m'
    )
  | L_comb -> (
      (* Right combs *)
      let aux (Label l , x) =
        let%bind t = compile_type x.associated_type in
        let annot_opt =
          match remove_empty_annotation x.michelson_annotation with
          | None -> Some l
          | annot -> annot
        in
        ok (t , annot_opt)
      in
      let rec lst_fold = function
        | [] -> fail (corner_case ~loc:__LOC__ "t_record empty")
        | [ x ] -> aux x
        | hd :: tl -> (
            let%bind (hd_t , hd_annot_opt) = aux hd in
            let%bind (tl_t , tl_annot_opt) = lst_fold tl in
            let%bind t = return @@ T_pair ((hd_annot_opt , hd_t) , (tl_annot_opt , tl_t)) in
            ok (t , None)
          )
      in
      let%bind (t , _ ) = lst_fold lst in
      ok t
    )

let record_access_to_lr ?(layout = L_tree) ty m_ty index =
  let open AST.Helpers in
  let lst = kv_list_of_t_record_or_tuple ~layout m_ty in
  match layout with
  | L_tree -> (
      let node_tv = Append_tree.of_list lst in
      let%bind path =
        let aux (i , _) = i = index in
        trace_option (corner_case ~loc:__LOC__ "record access leaf") @@
        Append_tree.exists_path aux node_tv
      in
      let lr_path = List.map (fun b -> if b then `Right else `Left) path in
      let%bind (_ , lst) =
        let aux = fun (ty , acc) cur ->
          let%bind (a , b) =
            trace_option (corner_case ~loc:__LOC__ "record access pair") @@
            Mini_c.get_t_pair ty
          in
          match cur with
          | `Left -> ok (a , acc @ [(a , `Left)])
          | `Right -> ok (b , acc @ [(b , `Right)] )
        in
        bind_fold_list aux (ty , []) lr_path
      in
      ok lst
    )
  | L_comb -> (
      let rec aux n ty last =
        match n , last with
        | 0 , true -> ok []
        | 0 , false -> (
            let%bind (a , _) =
              trace_option (corner_case ~loc:__LOC__ "record access pair") @@
              Mini_c.get_t_pair ty
            in
            ok [(a , `Left)]
          )
        | n , last -> (
            let%bind (_ , b) =
              trace_option (corner_case ~loc:__LOC__ "record access pair") @@
              Mini_c.get_t_pair ty
            in
            let%bind prec = aux (n - 1) b last in
            ok (prec @ [(b , `Right)])
          )
      in
      let%bind index =
        Trace.generic_try (corner_case ~loc:__LOC__ "record access index") @@
        fun () -> List.find_index (fun (label , _) -> label = index) lst
      in
      let last = (index + 1 = List.length lst) in
      aux index ty last
    )
  
let record_to_pairs compile_expression (return:?tv:_ -> _) record_t record : Mini_c.expression spilling_result =
  let open AST.Helpers in
  let lst = kv_list_of_record_or_tuple ?layout:record_t.layout_opt record_t.content record in
  match record_t.layout_opt with
  | Some L_tree | None -> (
    let node = Append_tree.of_list lst in
    let aux a b : (expression , spilling_error) result =
      let%bind a = a in
      let%bind b = b in
      let a_ty = Combinators.Expression.get_type a in
      let b_ty = Combinators.Expression.get_type b in
      let tv   = Combinators.Expression.make_t @@ T_pair ((None, a_ty) , (None, b_ty)) in
      return ~tv @@ ec_pair a b
    in
    trace_strong (corner_case ~loc:__LOC__ "record build") @@
    Append_tree.fold_ne (compile_expression) aux node
  )
  | Some L_comb -> (
    let rec aux = function
    | [] -> fail (corner_case ~loc:__LOC__ "record build")
    | [x] -> compile_expression x
    | [ a ; b ] -> (
      let%bind a' = compile_expression a in
      let%bind b' = compile_expression b in
      let tv = t_pair (None,a'.type_expression) (None,b'.type_expression) in
      return ~tv (ec_pair a' b')
    )
    | hd::tl -> (
      let%bind hd' = compile_expression hd in
      let%bind tl' = aux tl in
      let tv = t_pair (None,hd'.type_expression) (None,tl'.type_expression) in
      return ~tv (ec_pair hd' tl')
    ) in
    aux lst
  )