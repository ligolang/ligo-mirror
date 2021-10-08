(* Lexical units *)

(* Note how lexical units are parameterised by the type of the tokens,
   because the lexer library must not make any assumption on the
   tokens. Extending the function [Make] to take a module [Token]
   would be wrong because it would constrain the module [Token] to a
   signature [Token.S], whereas [Token] on the client-side has a much
   richer signature: we need parametric polymorphism on values for the
   tokens here. *)

type 'token lex_unit = [
  `Token     of 'token
| `Markup    of Markup.t
| `Directive of Directive.t
]

type 'token t = 'token lex_unit

(* Printing *)

type 'token formatter =
  offsets:bool -> [`Byte | `Point] -> 'token -> string

val print_tokens :
  offsets:bool ->
  [`Byte | `Point] ->
  token_to_string:('token formatter) ->
  Buffer.t ->
  'token t ->
  unit

val print_units :
  offsets:bool ->
  [`Byte | `Point] ->
  token_to_string:('token formatter) ->
  Buffer.t ->
  'token t ->
  unit

val print_copy :
  token_to_lexeme:('token -> string) ->
  Buffer.t ->
  'token t ->
  unit
