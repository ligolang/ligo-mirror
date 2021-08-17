(* Vendors dependencies *)

module Config  = Preprocessor.Config
module Options = LexerLib.Options
module Trace   = Simple_utils.Trace

(* Making lexers *)

module type S =
  sig
    module Token : Token.S

    (* Some inputs *)

    type file_path = string
    type directories = file_path list

    (* Results *)

    module Errors = Errors

    type raise = Errors.t Trace.raise

    (* Lexing various sources *)

    val from_file    : raise:raise -> directories -> file_path  -> Token.t list
    val from_string  : raise:raise -> directories -> string     -> Token.t list
    val from_buffer  : raise:raise -> directories -> Buffer.t   -> Token.t list
    val from_channel : raise:raise -> directories -> in_channel -> Token.t list

    (* Aliases *)

    val lex_file    : raise:raise -> directories -> file_path  -> Token.t list
    val lex_string  : raise:raise -> directories -> string     -> Token.t list
    val lex_buffer  : raise:raise -> directories -> Buffer.t   -> Token.t list
    val lex_channel : raise:raise -> directories -> in_channel -> Token.t list
  end

module Make (Config : Config.S) (Token : Token.S)
       : S with module Token = Token
