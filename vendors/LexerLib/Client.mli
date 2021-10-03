module Region = Simple_utils.Region

module type S =
  sig
    module State : State.S

    type message = string Region.reg

    type 'token lexer =
      'token State.t ->
      Lexing.lexbuf ->
      ('token * 'token State.t, message) Stdlib.result

    val mk_string : Thread.t -> 'token
    val callback  : 'token lexer
  end
