module type S =
  sig
    type token

    type message = string Simple_utils.Region.reg

    type error = token Unit.t list * message

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, error) Stdlib.result

    val mk_string : Thread.t -> token
    val callback  : lexer
  end
