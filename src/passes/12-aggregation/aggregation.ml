module Errors = Errors

let compile_expression ~raise : Ast_typed.expression -> Ast_aggregated.expression =
  fun x -> Compiler.compile_expression ~raise Compiler.Mod_env.empty x 
let compile_expression_in_context : Ast_typed.expression -> Ast_typed.expression Ast_aggregated.program -> Ast_aggregated.expression =
  fun i_exp prg -> prg i_exp

let compile_program ~raise : Ast_typed.module_fully_typed -> Ast_typed.expression Ast_aggregated.program =
  fun prg ->
    (fun hole -> Compiler.compile ~raise hole prg)

let compile_type ~raise : Ast_typed.type_expression -> Ast_aggregated.type_expression =
  fun ty -> Compiler.compile_type ~raise Compiler.Mod_env.empty ty