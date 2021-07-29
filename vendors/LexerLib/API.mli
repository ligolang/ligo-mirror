(* Using Core to make UTF-8 aware lexers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* CLIENT-SIDE (see README.md) *)

(* These types and values are re-exported from [Core] *)

type message = string Region.reg

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

(* FUNCTOR *)

(* Generic signature of input lexers *)

module type LEXER =
  sig
    type token

    val scan : token scanner
  end

(* The functor itself *)

module type S =
  sig
    (* The traditional API offers functions to lex various inputs and
       return token instances (see type [Core.instance]). *)

    type token

    type file_path = string
    type message   = string Region.reg

    type ('src,'dst) lexer =
      token State.config ->
      'src ->
      ('dst, message) Stdlib.result

    val from_lexbuf  : (Lexing.lexbuf, token Core.instance) lexer
    val from_channel : (in_channel,    token Core.instance) lexer
    val from_string  : (string,        token Core.instance) lexer
    val from_buffer  : (Buffer.t,      token Core.instance) lexer
    val from_file    : (file_path,     token Core.instance) lexer

    (* The advanced API offers functions to lex all tokens from all
       sources (module [Tokens]) or to lex all lexical units (module
       [Units]), that is, tokens and markup (see module [Markup]. *)

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
        type nonrec 'src lexer = ('src, token State.lex_unit list) lexer

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end
  end

module Make (Lexer : LEXER) : S with type token = Lexer.token

(* LEXER ENGINE *)

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. This function is useful when lexing a file that has been
   previously preprocessed, in which case the argument [file] is the
   name of the file that was preprocessed, _not_ the preprocessed file
   (of which the user is not normally aware). *)

type file_path = string

val reset :
  ?file:file_path ->
  ?line:int ->
  ?offset:int ->
  Lexing.lexbuf ->
  unit
