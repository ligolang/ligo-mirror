(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor return signature *)

module type S =
  sig
    (* Lexical units *)

    type lex_unit

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    (* LEXER INSTANCE (see README.md) *)

    type input =
      File    of file_path
    | String  of file_path * string
    | Channel of file_path * in_channel
    | Lexbuf  of file_path * Lexing.lexbuf

    type units = lex_unit list

    type error = {
      used_units : units;
      message    : message
    }

    type instance = {
      input      : input;
      read_units : Lexing.lexbuf -> (units, error) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit
    }

    val open_stream : input -> (instance, message) result
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S)
       : S with type lex_unit = Client.token Unit.t
