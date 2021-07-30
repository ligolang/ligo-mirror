(* Signature for the lexer client *)

module Region = Simple_utils.Region

type message = string Region.reg

type 'token scanner =
  'token State.t ->
  Lexing.lexbuf ->
  ('token State.lex_unit * 'token State.t, message) Stdlib.result

type 'token cut =
  Thread.t * 'token State.t -> 'token State.lex_unit * 'token State.t

type 'token t = <
  mk_string                : 'token cut;
  mk_eof                   : 'token scanner;
  callback                 : 'token scanner;
  support_string_delimiter : char -> bool
>

type 'token client = 'token t
