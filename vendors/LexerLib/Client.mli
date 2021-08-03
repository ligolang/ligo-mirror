(* Signature for the lexer client *)

module Region = Simple_utils.Region

type message = string Region.reg

type 'token scanner =
  'token State.t ->
  Lexing.lexbuf ->
  ('token * 'token State.t, message) Stdlib.result

type 'token cut =
  Thread.t * 'token State.t -> 'token * 'token State.t

type 'token client = <
  mk_string           : 'token cut;
  callback            : 'token scanner;
  is_string_delimiter : string -> bool
>

type 'token t = 'token client
