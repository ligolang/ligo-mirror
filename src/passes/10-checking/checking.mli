
module I = Ast_core
module O = Ast_typed

module Errors = Errors
open Errors
open Simple_utils.Trace

module Context = Context

val type_program     : raise:typer_error raise -> test:bool -> protocol_version:Environment.Protocols.t -> stdlib:Environment.t -> I.module_ -> O.module_fully_typed
val type_declaration : raise:typer_error raise -> test:bool -> protocol_version:Environment.Protocols.t -> stdlib:Environment.t -> I.declaration Location.wrap -> O.declaration Location.wrap
val type_expression  : raise:typer_error raise -> test:bool -> protocol_version:Environment.Protocols.t -> ?env:Environment.t -> ?tv_opt:O.type_expression -> I.expression -> O.expression


val untype_type_expression : O.type_expression -> I.type_expression
val untype_expression : O.expression -> I.expression

val untype_module_fully_typed : O.module_fully_typed -> I.module_

val assert_type_expression_eq : raise:typer_error raise -> Location.t -> O.type_expression * O.type_expression -> unit
