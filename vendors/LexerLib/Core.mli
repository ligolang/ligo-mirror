(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The functor return signature *)

module type S =
  sig
    type token

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    (* LEXER INSTANCE (see README.md) *)

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type instance = {
      input      : input;
      read_token : Lexing.lexbuf -> (token, message) result;
      read_unit  : Lexing.lexbuf -> (token Unit.t, message) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit;
      window     : unit -> token State.window option
    }

    val open_stream : input -> (instance, message) Stdlib.result
  end

(* THE FUNCTOR *)

module Make (Config  : Preprocessor.Config.S)
            (Options : Options.S)
            (Token   : Token.S)
            (Client  : Client.S with type token = Token.token)
       : S with type token = Token.t
