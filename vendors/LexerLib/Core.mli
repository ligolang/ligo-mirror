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

(* LEXER ENGINE *)

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. This function is useful when lexing a file that has been
   previously preprocessed, in which case the argument [file] is the
   name of the file that was preprocessed, _not_ the preprocessed file
   (of which the user is not normally aware). *)

val reset :
  ?file:string ->
  ?line:int ->
  ?offset:int ->
  Lexing.lexbuf ->
  unit
