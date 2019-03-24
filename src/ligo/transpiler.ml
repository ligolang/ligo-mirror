open! Ligo_helpers.Trace
open Mini_c
open Combinators

module AST = Ast_typed
open AST.Combinators

let list_of_map m = List.rev @@ Ligo_helpers.X_map.String.fold (fun _ v prev -> v :: prev) m []
let kv_list_of_map m = List.rev @@ Ligo_helpers.X_map.String.fold (fun k v prev -> (k, v) :: prev) m []
let map_of_kv_list lst =
  let open AST.SMap in
  List.fold_left (fun prev (k, v) -> add k v prev) empty lst

let rec translate_type (t:AST.type_value) : type_value result =
  match t.type_value with
  | Type_constant ("bool", []) -> ok (`Base Bool)
  | Type_constant ("int", []) -> ok (`Base Int)
  | Type_constant ("string", []) -> ok (`Base String)
  | Type_constant _ -> simple_fail "unrecognized constant"
  | Type_sum m ->
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (`Or (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | Type_record m ->
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (`Pair (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | Type_tuple lst ->
      let node = Append_tree.of_list lst in
      let aux a b : type_value result =
        let%bind a = a in
        let%bind b = b in
        ok (`Pair (a, b))
      in
      Append_tree.fold_ne translate_type aux node
  | Type_function (param, result) ->
      let%bind param' = translate_type param in
      let%bind result' = translate_type result in
      ok (`Function (param', result'))

let rec translate_block env (b:AST.block) : block result =
  let env' = Environment.extend env in
  let%bind (instructions, env') =
    let rec aux e acc lst = match lst with
      | [] -> ok (acc, e)
      | hd :: tl ->
          match%bind translate_instruction e hd with
          | Some ((_, e') as i) -> aux e'.post_environment (i :: acc) tl
          | None -> aux e acc tl
    in
    let%bind (lst, e) = aux env' [] b in
    ok (List.rev lst, e)
  in
  ok (instructions, environment_wrap env env')

and translate_instruction (env:Environment.t) (i:AST.instruction) : statement option result =
  let return ?(env' = env) x : statement option result = ok (Some (x, environment_wrap env env')) in
  match i with
  | Assignment {name;annotated_expression} ->
      let%bind (_, t, _) as expression = translate_annotated_expression env annotated_expression in
      let env' = Environment.add (name, t) env in
      return ~env' (Assignment (name, expression))
  | Matching (expr, Match_bool {match_true ; match_false}) ->
      let%bind expr' = translate_annotated_expression env expr in
      let%bind true_branch = translate_block env match_true in
      let%bind false_branch = translate_block env match_false in
      return (Cond (expr', true_branch, false_branch))
  | Matching _ -> simple_fail "todo : match"
  | Loop (expr, body) ->
      let%bind expr' = translate_annotated_expression env expr in
      let%bind body' = translate_block env body in
      return (While (expr', body'))
  | Skip -> ok None
  | Fail _ -> simple_fail "todo : fail"

and translate_annotated_expression (env:Environment.t) (ae:AST.annotated_expression) : expression result =
  let%bind tv = translate_type ae.type_annotation in
  match ae.expression with
  | Literal (Bool b) -> ok (Literal (`Bool b), tv, env)
  | Literal (Int n) -> ok (Literal (`Int n), tv, env)
  | Literal (Nat n) -> ok (Literal (`Nat n), tv, env)
  | Literal (Bytes s) -> ok (Literal (`Bytes s), tv, env)
  | Literal (String s) -> ok (Literal (`String s), tv, env)
  | Literal Unit -> ok (Literal `Unit, tv, env)
  | Variable name -> ok (Var name, tv, env)
  | Application (a, b) ->
      let%bind a = translate_annotated_expression env a in
      let%bind b = translate_annotated_expression env b in
      ok (Apply (a, b), tv, env)
  | Constructor (m, param) ->
      let%bind (param'_expr, param'_tv, _) = translate_annotated_expression env ae in
      let%bind map_tv = get_t_sum ae.type_annotation in
      let node_tv = Append_tree.of_list @@ kv_list_of_map map_tv in
      let%bind ae' =
        let leaf (k, tv) : (expression' option * type_value) result =
          if k = m then (
            let%bind _ =
              trace (simple_error "constructor parameter doesn't have expected type (shouldn't happen here)")
              @@ AST.assert_type_value_eq (tv, param.type_annotation) in
            ok (Some (param'_expr), param'_tv)
          ) else (
            let%bind tv = translate_type tv in
            ok (None, tv)
          ) in
        let node a b : (expression' option * type_value) result =
          let%bind a = a in
          let%bind b = b in
          match (a, b) with
          | (None, a), (None, b) -> ok (None, `Or (a, b))
          | (Some _, _), (Some _, _) -> simple_fail "several identical constructors in the same variant (shouldn't happen here)"
          | (Some v, a), (None, b) -> ok (Some (Predicate ("LEFT", [v, a, env])), `Or (a, b))
          | (None, a), (Some v, b) -> ok (Some (Predicate ("RIGHT", [v, b, env])), `Or (a, b))
        in
        let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
        let%bind ae =
          trace_option (simple_error "constructor doesn't exist in claimed type (shouldn't happen here)")
            ae_opt in
        ok (ae, tv, env) in
      ok ae'
  | Tuple lst ->
      let node = Append_tree.of_list lst in
      let aux (a:expression result) (b:expression result) : expression result =
        let%bind (_, a_ty, _) as a = a in
        let%bind (_, b_ty, _) as b = b in
        ok (Predicate ("PAIR", [a; b]), `Pair(a_ty, b_ty), env)
      in
      Append_tree.fold_ne (translate_annotated_expression env) aux node
  | Tuple_accessor (tpl, ind) ->
      let%bind (tpl'_expr, _, _) = translate_annotated_expression env tpl in
      let%bind tpl_tv = get_t_tuple ae.type_annotation in
      let node_tv = Append_tree.of_list @@ List.mapi (fun i a -> (a, i)) tpl_tv in
      let%bind ae' =
        let leaf (tv, i) : (expression' option * type_value) result =
          let%bind tv = translate_type tv in
          if i = ind then (
            ok (Some (tpl'_expr), tv)
          ) else (
            ok (None, tv)
          ) in
        let node a b : (expression' option * type_value) result =
          let%bind a = a in
          let%bind b = b in
          match (a, b) with
          | (None, a), (None, b) -> ok (None, `Pair (a, b))
          | (Some _, _), (Some _, _) -> simple_fail "several identical indexes in the same tuple (shouldn't happen here)"
          | (Some v, a), (None, b) -> ok (Some (Predicate ("CAR", [v, a, env])), `Pair (a, b))
          | (None, a), (Some v, b) -> ok (Some (Predicate ("CDR", [v, b, env])), `Pair (a, b))
        in
        let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
        let%bind ae = trace_option (simple_error "bad index in tuple (shouldn't happen here)")
            ae_opt in
        ok (ae, tv, env) in
      ok ae'
  | Record m ->
      let node = Append_tree.of_list @@ list_of_map m in
      let aux a b : expression result =
        let%bind (_, a_ty, _) as a = a in
        let%bind (_, b_ty, _) as b = b in
        ok (Predicate ("PAIR", [a; b]), `Pair(a_ty, b_ty), env)
      in
      Append_tree.fold_ne (translate_annotated_expression env) aux node
  | Record_accessor (r, key) ->
      let%bind (r'_expr, _, _) = translate_annotated_expression env r in
      let%bind r_tv = get_t_record ae.type_annotation in
      let node_tv = Append_tree.of_list @@ kv_list_of_map r_tv in
      let%bind ae' =
        let leaf (key', tv) : (expression' option * type_value) result =
          let%bind tv = translate_type tv in
          if key = key' then (
            ok (Some (r'_expr), tv)
          ) else (
            ok (None, tv)
          ) in
        let node a b : (expression' option * type_value) result =
          let%bind a = a in
          let%bind b = b in
          match (a, b) with
          | (None, a), (None, b) -> ok (None, `Pair (a, b))
          | (Some _, _), (Some _, _) -> simple_fail "several identical keys in the same record (shouldn't happen here)"
          | (Some v, a), (None, b) -> ok (Some (Predicate ("CAR", [v, a, env])), `Pair (a, b))
          | (None, a), (Some v, b) -> ok (Some (Predicate ("CDR", [v, b, env])), `Pair (a, b))
        in
        let%bind (ae_opt, tv) = Append_tree.fold_ne leaf node node_tv in
        let%bind ae = trace_option (simple_error "bad key in record (shouldn't happen here)")
            ae_opt in
        ok (ae, tv, env) in
      ok ae'
  | Constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (translate_annotated_expression env) lst in
      ok (Predicate (name, lst'), tv, env)
  | Lambda l -> translate_lambda env l tv

and translate_lambda env l tv =
  let { binder ; input_type ; output_type ; body ; result } : AST.lambda = l in
  (* Try to type it in an empty env, if it succeeds, transpiles it as a quote value, else, as a closure expression. *)
  let%bind empty_env =
    let%bind input = translate_type input_type in
    ok Environment.(add (binder, input) empty) in
  match to_option (translate_block empty_env body), to_option (translate_annotated_expression empty_env result) with
  | Some body, Some result ->
      let capture_type = No_capture in
      let%bind input = translate_type input_type in
      let%bind output = translate_type output_type in
      let content = {binder;input;output;body;result;capture_type} in
      ok (Literal (`Function {capture=None;content}), tv, env)
  | _ ->
      (* Shallow capture. Capture the whole environment. Extend it with a new scope. Append it the input. *)
      let%bind input = translate_type input_type in
      let sub_env = Environment.extend env in
      let full_env = Environment.add (binder, input) sub_env in
      let%bind (_, e) as body = translate_block full_env body in
      let%bind result = translate_annotated_expression e.post_environment result in
      let capture_type = Shallow_capture sub_env in
      let input = Environment.to_mini_c_type full_env in
      let%bind output = translate_type output_type in
      let content = {binder;input;output;body;result;capture_type} in
      ok (Function_expression content, tv, env)

let translate_declaration env (d:AST.declaration) : toplevel_statement result =
  match d with
  | Constant_declaration {name;annotated_expression} ->
      let%bind ((_, tv, _) as expression) = translate_annotated_expression env annotated_expression in
      let env' = Environment.add (name, tv) env in
      ok @@ ((name, expression), environment_wrap env env')

let translate_program (lst:AST.program) : program result =
  let aux (prev:(toplevel_statement list * Environment.t) result) cur =
    let%bind (tl, env) = prev in
    let%bind ((_, env') as cur') = translate_declaration env cur in
    ok (cur' :: tl, env'.post_environment)
  in
  let%bind (statements, _) = List.fold_left aux (ok ([], Environment.empty)) lst in
  ok statements

let translate_main (l:AST.lambda) (t:AST.type_value) : anon_function result =
  let%bind t' = translate_type t in
  let%bind (expr, _, _) = translate_lambda Environment.empty l t' in
  match expr with
  | Literal (`Function f) -> ok f
  | _ -> simple_fail "main is not a function"

open Combinators

let rec exp x n =
  if n = 0
  then 1
  else
    let exp' = exp (x * x) (n / 2) in
    let m = if n mod 2 = 0 then 1 else x in
    m * exp'

let exp2 = exp 2

let extract_constructor (v : value) (tree : _ Append_tree.t') : (string * value * AST.type_value) result =
  let open Append_tree in
  let rec aux tv : (string * value * AST.type_value) result=
    match tv with
    | Leaf (k, t), v -> ok (k, v, t)
    | Node {a}, `Left v -> aux (a, v)
    | Node {b}, `Right v -> aux (b, v)
    | _ -> simple_fail "bad constructor path"
  in
  let%bind (s, v, t) = aux (tree, v) in
  ok (s, v, t)

let extract_tuple (v : value) (tree : AST.type_value Append_tree.t') : ((value * AST.type_value) list) result =
  let open Append_tree in
  let rec aux tv : ((value * AST.type_value) list) result =
    match tv with
    | Leaf t, v -> ok @@ [v, t]
    | Node {a;b}, `Pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> simple_fail "bad tuple path"
  in
  aux (tree, v)

let extract_record (v : value) (tree : _ Append_tree.t') : (_ list) result =
  let open Append_tree in
  let rec aux tv : ((string * (value * AST.type_value)) list) result =
    match tv with
    | Leaf (s, t), v -> ok @@ [s, (v, t)]
    | Node {a;b}, `Pair (va, vb) ->
        let%bind a' = aux (a, va) in
        let%bind b' = aux (b, vb) in
        ok (a' @ b')
    | _ -> simple_fail "bad record path"
  in
  aux (tree, v)


let rec untranspile (v : value) (t : AST.type_value) : AST.annotated_expression result =
  let open! AST in
  let return e = ok AST.(annotated_expression e t) in
  match t.type_value with
  | Type_constant ("bool", []) ->
      let%bind b = get_bool v in
      return (Literal (Bool b))
  | Type_constant ("int", []) ->
      let%bind n = get_int v in
      return (Literal (Int n))
  | Type_constant ("string", []) ->
      let%bind n = get_string v in
      return (Literal (String n))
  | Type_constant _ ->
      simple_fail "unknown type_constant"
  | Type_sum m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> simple_fail "empty sum type"
        | Full t -> ok t
      in
      let%bind (name, v, tv) = extract_constructor v node in
      let%bind sub = untranspile v tv in
      return (Constructor (name, sub))
  | Type_tuple lst ->
      let%bind node = match Append_tree.of_list lst with
        | Empty -> simple_fail "empty tuple"
        | Full t -> ok t in
      let%bind tpl = extract_tuple v node in
      let%bind tpl' = bind_list
        @@ List.map (fun (x, y) -> untranspile x y) tpl in
      return (Tuple tpl')
  | Type_record m ->
      let lst = kv_list_of_map m in
      let%bind node = match Append_tree.of_list lst with
        | Empty -> simple_fail "empty record"
        | Full t -> ok t in
      let%bind lst = extract_record v node in
      let%bind lst = bind_list
        @@ List.map (fun (x, (y, z)) -> let%bind yz = untranspile y z in ok (x, yz)) lst in
      let m' = map_of_kv_list lst in
      return (Record m')
  | Type_function _ -> simple_fail "no untranspilation for functions yet"
