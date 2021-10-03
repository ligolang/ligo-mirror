(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor return signature *)

module type S =
  sig
    type 'token lex_unit

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    (* LEXER INSTANCE (see README.md) *)

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type 'token units = 'token lex_unit list

    type 'token error = {
      used_units : 'token units;
      message    : message
    }

    type 'token instance = {
      input      : input;
      read_units : Lexing.lexbuf -> ('token units, 'token error) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit
    }

    val open_stream : input -> ('token instance, message) result
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S)
       : S with type 'token lex_unit = 'token Client.State.Unit.t
