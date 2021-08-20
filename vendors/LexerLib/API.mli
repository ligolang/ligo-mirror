(* Specialising the Core lexer for the library clients *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor's return signature *)

module type S =
  sig
    type token

    type file_path = string
    type message   = string Region.reg

    type ('src,'dst) lexer = 'src -> ('dst, message) Stdlib.result

    module Tokens :
      sig
        val from_lexbuf  : (Lexing.lexbuf, token list) lexer
        val from_channel : (in_channel,    token list) lexer
        val from_string  : (string,        token list) lexer
        val from_buffer  : (Buffer.t,      token list) lexer
        val from_file    : (file_path,     token list) lexer
      end

    module LexUnits :
      sig
        type nonrec 'src lexer = ('src, token Unit.t list) lexer

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
