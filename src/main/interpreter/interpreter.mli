type interpreter_error = Errors.interpreter_error

val eval_test   : raise:interpreter_error Trace.raise -> steps:int -> protocol_version:Environment.Protocols.t -> Ast_typed.module_fully_typed -> (Ast_typed.expression -> Ast_aggregated.expression) -> (Ast_aggregated.module_variable * Ligo_interpreter.Types.value) list
