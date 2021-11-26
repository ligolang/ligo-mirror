type interpreter_error = Errors.interpreter_error

val eval_module : raise:interpreter_error Trace.raise 
    -> steps:int
    -> options:Compiler_options.t
    -> Ast_typed.module_fully_typed * Tezos_state.context * Ligo_interpreter.Types.env
    -> Ligo_interpreter.Types.env * Tezos_state.context
val eval_test   : raise:interpreter_error Trace.raise 
    -> steps:int 
    -> options:Compiler_options.t
    -> Ast_typed.module_fully_typed -> Ligo_interpreter.Types.env * ((string * Ligo_interpreter.Types.value) list)
