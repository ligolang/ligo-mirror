
module I = Ast_core
module O = Ast_typed

module Errors = Errors
open Errors
open Simple_utils.Trace

val type_program     : raise:typer_error raise -> add_warning : ([> `Checking_ambiguous_contructor of Location.t * Stage_common.Types.type_variable * Stage_common.Types.type_variable ] -> unit) ->
  options:Compiler_options.middle_end -> ?env:Environment.t -> I.module_ -> O.program
val type_declaration : raise:typer_error raise -> add_warning : ([> `Checking_ambiguous_contructor of Location.t * Stage_common.Types.type_variable * Stage_common.Types.type_variable ] -> unit) ->
  options:Compiler_options.middle_end -> ?env:Environment.t -> I.declaration -> O.declaration
val type_expression  : raise:typer_error raise -> add_warning : ([> `Checking_ambiguous_contructor of Location.t * Stage_common.Types.type_variable * Stage_common.Types.type_variable ] -> unit) ->
  options:Compiler_options.middle_end -> ?env:Environment.t -> ?tv_opt:O.type_expression -> I.expression -> O.expression


val untype_expression : O.expression -> I.expression
val untype_program : O.program -> I.module_

val assert_type_expression_eq : raise:typer_error raise -> Location.t -> O.type_expression * O.type_expression -> unit
