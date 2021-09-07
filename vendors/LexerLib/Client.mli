module type S =
  sig
    type token

    type message = string Simple_utils.Region.reg

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    val mk_string : Thread.t -> token
    val callback  : lexer
  end
