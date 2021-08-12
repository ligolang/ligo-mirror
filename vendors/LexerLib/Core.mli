(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* The signature of client lexers *)

module type CLIENT =
  sig
    type token

    type message = string Simple_utils.Region.reg

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    val mk_string           : Thread.t -> token
    val callback            : lexer
    val is_string_delimiter : string -> bool
  end

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

(* General configuration *)

module type CONFIG = module type of Preprocessor.Config

(* CLI options *)

module type OPTIONS = module type of Options

(* The signature of tokens *)

module type TOKEN = module type of Token

(* The functor signature *)

module Make (Config  : CONFIG)
            (Options : OPTIONS)
            (Token   : TOKEN)
            (Client  : CLIENT with type token = Token.t)
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
