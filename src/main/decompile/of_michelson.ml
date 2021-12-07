module Formatter = Formatter

open Main_errors
open Trace
open Simple_utils.Runned_result

let decompile_value ~raise (output_type:Ast_aggregated.type_expression) (ty, value) =
  let mini_c     = trace ~raise decompile_michelson @@ Stacking.Decompiler.decompile_value ty value in
  let aggregated =  trace ~raise decompile_mini_c    @@ Spilling.decompile mini_c output_type in
  let typed      =  trace ~raise decompile_aggregated @@ Aggregation.decompile aggregated in
  let inferred   = Checking.untype_expression typed in
  let core       = Inference.Untyper.untype_expression inferred in
  core

(* this function is used by - today dry-run - when the application of a contract to its arguments
   is done at michelson level (i.e. pushing the arguments onto the stack).
*)
let decompile_value_from_contract_execution ~raise (output_type: Ast_aggregated.type_expression) runned_result =
  match runned_result with
  | Fail s -> Fail s
  | Success ex_ty_value ->
    let (_,return_type) = trace_option ~raise entrypoint_not_a_function @@ Ast_aggregated.get_t_function output_type in
    let decompiled_value = decompile_value ~raise return_type ex_ty_value in
    (Success decompiled_value)

let decompile_expression ~raise (type_value: Ast_aggregated.type_expression) runned_result =
  match runned_result with
  | Fail s -> Fail s
  | Success ex_ty_value ->
    let decompiled_value = decompile_value ~raise type_value ex_ty_value in
    (Success decompiled_value)
