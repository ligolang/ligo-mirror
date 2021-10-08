(* Specialising the Core lexer for the library clients *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor's return signature *)

module type S =
  sig
    type lex_unit

    type units = lex_unit list

    type error = {
      used_units : units;
      message    : string Region.reg
    }

    type 'src lexer = 'src -> (units, error) result

    val from_lexbuf  : file:string -> Lexing.lexbuf lexer
    val from_channel : file:string -> in_channel lexer
    val from_string  : file:string -> string lexer
    val from_file    : string lexer
    val from_buffer  : Buffer.t lexer
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S)
       : S with type lex_unit = Client.token Unit.t
