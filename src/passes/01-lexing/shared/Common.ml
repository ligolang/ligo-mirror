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

module Make (Config : Config.S) (Token : Token.S) =
  struct
    module Token = Token

    (* Some inputs *)

    type file_path = string
    type directories = file_path list

    (* Results *)

    module Errors = Errors

    type raise = Errors.t Trace.raise

    (* Instantiating the client lexer *)

    module Client = Lexer.Make (Token)

    (* Partially instantiating the final lexer *)

    module Scan (Options : Options.S) =
      LexerLib.API.Make (Config) (Options) (Token) (Client)

    (* Partial option module *)

    module PreOptions = (* TODO Flow from the compiler CLI *)
      struct
        let show_pp    = false
        let offsets    = true
        let preprocess = true
        let mode       = `Point
        let command    = None
      end

    (* Lifting [Stdlib.result] to [Trace.result]. *)

    let lift ~(raise:raise) = function
      Ok tokens -> tokens
    | Error msg -> raise.raise @@ Errors.generic msg

    (* Lexing a file *)

    let from_file ~raise dirs file_path =
      let module Options =
        struct
          let input = Some file_path
          let dirs  = dirs
          include PreOptions
        end in
      let open Scan (Options)
      in Tokens.from_file file_path |> lift ~raise

    let lex_file = from_file

    (* Lexing a string *)

    let from_string ~raise dirs string =
     let module Options =
        struct
          let input = None
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in Tokens.from_string string |> lift ~raise

    let lex_string = from_string

    (* Lexing a string buffer *)

    let from_buffer ~raise dirs buffer =
     let module Options =
        struct
          let input = None
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in Tokens.from_buffer buffer |> lift ~raise

    let lex_buffer = from_buffer

    (* Lexing an input channel *)

    let from_channel ~raise dirs channel =
     let module Options =
        struct
          let input = None
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in Tokens.from_channel channel |> lift ~raise

    let lex_channel = from_channel
  end
