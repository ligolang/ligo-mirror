(* A library for writing UTF8-aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module FQueue = Simple_utils.FQueue

(* Utility types *)

type file_path = string
type message = string Region.reg

(* LEXER ENGINE *)

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. This function is useful when lexing a file that has been
   previously preprocessed, in which case the argument [file] is the
   name of the file that was preprocessed, _not_ the preprocessed file
   (of which the user is not normally aware). *)

val reset :
  ?file:file_path ->
  ?line:int ->
  ?offset:int ->
  Lexing.lexbuf ->
  unit

(* CLIENT-SIDE (see README.md) *)

type 'token scanner =
  'token State.t ->
  Lexing.lexbuf ->
  ('token State.lex_unit * 'token State.t, message) Stdlib.result

type 'token cut =
  Thread.t * 'token State.t -> 'token State.lex_unit * 'token State.t

(* The type [client] gathers the arguments to the lexer in this
    module. *)

type 'token client = <
  mk_string                : 'token cut;
  mk_eof                   : 'token scanner;
  callback                 : 'token scanner;
  support_string_delimiter : char -> bool
>

val mk_scan : 'token client -> 'token scanner

(* LEXER INSTANCE (see README.md) *)

type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type 'token instance = {
  input      : input;
  read_token : Lexing.lexbuf -> ('token, message) result;
  read_unit  : Lexing.lexbuf -> ('token State.lex_unit, message) result;
  lexbuf     : Lexing.lexbuf;
  close      : unit -> unit;
  window     : unit -> 'token State.window option
}

val open_stream :
  'token State.config ->
  scan:('token scanner) ->
  input ->
   ('token instance, message) Stdlib.result
