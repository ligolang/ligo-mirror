(* Vendors dependencies *)

module Trace   = Simple_utils.Trace
module Config  = Preprocessor.Config
module Options = LexerLib.Options
module Unit    = LexerLib.Unit

(* Common type definition *)

type lexeme = string

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

    (* TODO: Do we want to pass the original file name, if any? *)

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

    (* Partially instantiating the final lexer *)

    module Scan (Options : Options.S) =
      LexerLib.API.Make (Config) (Lexer.Make (Options) (Token))

    (* Partial option module *)

    module PreOptions = (* TODO Flow from the compiler CLI *)
      struct
        type post_pass = Pass of int | All

        let show_pp     = false
        let offsets     = true
        let preprocess  = true
        let postprocess = Some All
        let mode        = `Point
        let command     = None
      end

    (* Filtering out the markup *)

    let filter_tokens units : Token.t list =
      let apply tokens = function
        `Token token -> token :: tokens
      | `Markup _    -> tokens
      | `Directive d -> Token.mk_directive d :: tokens
      in List.fold_left apply [] units |> List.rev

    (* Lexing a file *)

    let from_file ~(raise:raise) dirs file =
      let module Options =
        struct
          let input = Some file
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in match from_file file with
          Ok units -> filter_tokens units
        | Error {message; _} ->
            raise.raise @@ Errors.generic message
    let lex_file = from_file

    (* Lexing a string *)

    let from_string ~(raise:raise) dirs string =
     let module Options =
        struct
          let input = None
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in match from_string ~file:"" string with
          Ok units -> filter_tokens units
        | Error {message; _} ->
            raise.raise @@ Errors.generic message

    let lex_string = from_string

    (* Lexing a string buffer *)

    let from_buffer ~(raise:raise) dirs buffer =
     let module Options =
        struct
          let input = None
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in match from_buffer buffer with
          Ok units -> filter_tokens units
        | Error {message; _} ->
            raise.raise @@ Errors.generic message

    let lex_buffer = from_buffer

    (* Lexing an input channel *)

    let from_channel ~(raise:raise) dirs channel =
     let module Options =
        struct
          let input = None
          let dirs  = dirs
          include PreOptions
        end in
     let open Scan (Options)
     in match from_channel ~file:"" channel with
          Ok units -> filter_tokens units
        | Error {message; _} ->
            raise.raise @@ Errors.generic message

    let lex_channel = from_channel
  end
