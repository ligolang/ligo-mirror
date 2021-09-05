(* Specialising the Core lexer for the library clients *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor's return signature *)

module type S =
  sig
    type token

    type file_path = string
    type message   = string Region.reg

    module Tokens :
    sig
        type tokens = token list
        type 'src lexer = 'src -> (tokens, tokens * message) Stdlib.result

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end

    module LexUnits :
      sig
        type units = token Unit.t list
        type 'src lexer = 'src -> (units, units * message) Stdlib.result

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end
  end

(* THE FUNCTOR *)

module Make (Config  : Preprocessor.Config.S)
            (Options : Options.S)
            (Token   : Token.S)
            (Client  : Client.S with type token = Token.token)
       : S with type token = Token.t
