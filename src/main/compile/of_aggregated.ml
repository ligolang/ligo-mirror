open Trace
open Ast_aggregated
open Spilling
open Main_errors

module SMap = Map.Make(String)

(* let compile_with_modules ~raise : Ast_typed.module_fully_typed -> Mini_c.program = fun p ->
  trace ~raise spilling_tracer @@ compile_module p

let compile ~raise : Ast_typed.module_fully_typed -> Mini_c.program = fun p ->
  let mini_c = compile_with_modules ~raise p in
  mini_c *)

let compile_expression ~raise : expression -> Mini_c.expression = fun e ->
  let e = Self_ast_aggregated.monomorphise_expression e in
  trace ~raise spilling_tracer @@ compile_expression e

let compile_type ~raise : type_expression -> Mini_c.type_expression = fun e ->
  trace ~raise spilling_tracer @@ compile_type e
