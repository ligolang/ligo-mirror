(* Signature for the lexer client *)

module Region = Simple_utils.Region

type message = string Region.reg

type 'token lexer =
  'token State.t ->
  Lexing.lexbuf ->
  ('token * 'token State.t, message) Stdlib.result

type 'token client = <
  mk_string           : Thread.t -> 'token;
  callback            : 'token lexer;
  is_string_delimiter : string -> bool
>

type 'token t = 'token client
