(* Lexical units *)

(* Note how lexical units are parameterised by the type of the tokens,
   because the lexer library must not make any assumption on the
   tokens. Extending the function [Make] to take a module [Token]
   would be wrong because it would constrain the module [Token] to a
   signature [Token.S], whereas [Token] on the client-side has a much
   richer signature: we need parametric polymorphism on values for the
   tokens here. *)

module type S =
  sig
    module Options : Options.S

    type 'token lex_unit = [
      `Token     of 'token
    | `Markup    of Markup.t
    | `Directive of Directive.t
    ]

    type 'token t = 'token lex_unit

    (* Filtering tokens from a list of lexical units *)

    val filter_tokens : 'token t list -> 'token list

    (* Printing *)

    val print :
      token_to_lexeme:('token -> string) ->
      token_to_string:(offsets:bool -> [`Byte | `Point] -> 'token -> string) ->
      Buffer.t ->
      'token t ->
      unit
  end

module Make (Options : Options.S) : S with module Options = Options
