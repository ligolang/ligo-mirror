open Main_errors
open Mini_c
open Proto_alpha_utils
open Trace
open! Stacking
open Tezos_micheline

let dummy_locations : 'l 'p. ('l, 'p) Micheline.node -> (Location.t, 'p) Micheline.node =
  fun e ->
  Micheline.(inject_locations (fun _ -> Location.dummy) (strip_locations e))

let compile_contract ~raise : options:Compiler_options.t -> expression -> Stacking.compiled_expression  = fun ~options e ->
  let () = Format.eprintf "ofminic bef : %a\n" PP.type_expression e.type_expression in
  let e = Self_mini_c.all_expression ~raise e in
  let () = Format.eprintf "ofminic bef' : %a\n" PP.type_expression e.type_expression in
  let (input_ty , _) = trace ~raise self_mini_c_tracer @@ Self_mini_c.get_t_function e.type_expression in
  let () = Format.eprintf "ofminic aft : %a\n" PP.type_expression e.type_expression in
  (* let body : anon_function = trace ~raise self_mini_c_tracer @@ Self_mini_c.get_function e in *)
  let body = Scoping.hello e in (* Binds (usages, [translate_type input_ty], body) *)
  let body = trace ~raise stacking_tracer @@ Stacking.Program.compile_expr options.protocol_version body in
  let expr = Self_michelson.optimize options.protocol_version body in
  let expr_ty = Scoping.translate_type e.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ({ expr_ty ; expr } : Stacking.Program.compiled_expression)

let compile_expression ~raise : options:Compiler_options.t -> expression -> compiled_expression = fun ~options e ->
  let e = Self_mini_c.all_expression ~raise e in
  let (expr, _) = Scoping.translate_expression e [] in
  let expr = trace ~raise stacking_tracer @@ Stacking.Program.compile_expr options.protocol_version [] [] expr in
  let expr = Self_michelson.optimize options.protocol_version expr in
  let expr_ty = Scoping.translate_type e.type_expression in
  let expr_ty = dummy_locations expr_ty in
  ({ expr_ty ; expr } : Program.compiled_expression)

let compile_type = fun e ->
  let expr_ty = Scoping.translate_type e in
  dummy_locations expr_ty

let pretty_print program =
  Mini_c.PP.expression program
