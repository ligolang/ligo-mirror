(* Specialising the Core lexer for the library clients *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor's return signature *)

module type S =
  sig
    type 'token lex_unit

    type file_path = string
    type message   = string Region.reg

    type 'token units = 'token lex_unit list

    type 'token error = {
      used_units : 'token units;
      message    : message
    }

    type ('token, 'src) lexer =
      'src -> ('token units, 'token error) result

    val from_lexbuf  : ('token, Lexing.lexbuf) lexer
    val from_channel : ('token, in_channel)    lexer
    val from_string  : ('token, string)        lexer
    val from_buffer  : ('token, Buffer.t)      lexer
    val from_file    : ('token, file_path)     lexer
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S)
       : S with type 'token lex_unit = 'token Client.State.Unit.t
