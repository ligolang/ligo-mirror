(* Vendor dependencies *)

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* The functor itself *)

module type S =
  sig
    type token

    type file_path = string
    type message   = string Region.reg

    module Tokens :
      sig
        type tokens = token list
        type 'src lexer = 'src -> (tokens, tokens * message) Stdlib.result

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end

    module LexUnits :
      sig
        type units = token Unit.t list
        type 'src lexer = 'src -> (units, units * message) Stdlib.result

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end
  end

(* THE FUNCTOR *)

module Make (Config  : Preprocessor.Config.S)
            (Options : Options.S)
            (Token   : Token.S)
            (Client  : Client.S with type token = Token.token) =
  struct
    module Core = Core.Make (Config) (Options) (Token) (Client)

    type token = Token.t

    type file_path = string
    type message   = string Region.reg

    (* Generic lexer for all kinds of inputs *)

    let generic lexbuf_of source =
      Core.(open_stream @@ Buffer (lexbuf_of source))

    (* Lexing the input to recognise one token *)

    let from_lexbuf  = generic (fun x -> x)
    let from_channel = generic Lexing.from_channel
    let from_string  = generic Lexing.from_string

    let from_buffer buffer = from_string @@ Buffer.contents buffer
    let from_file path = Core.(open_stream (File path))

    (* Lexing the entire input *)

    module Tokens =
      struct
        type tokens = token list
        type 'src lexer = 'src -> (tokens, tokens * message) Stdlib.result

        let scan_all_tokens = function
          Stdlib.Error msg ->
            flush_all (); Stdlib.Error ([], msg)
        | Ok Core.{read_tokens; lexbuf; close; _} ->
            let close_all () = flush_all (); close ()
            and result = read_tokens lexbuf
            in close_all (); result

        let from_lexbuf   lexbuf = from_lexbuf   lexbuf |> scan_all_tokens
        let from_channel channel = from_channel channel |> scan_all_tokens
        let from_string   string = from_string   string |> scan_all_tokens
        let from_buffer   buffer = from_buffer   buffer |> scan_all_tokens
        let from_file        src = from_file        src |> scan_all_tokens
      end

    module LexUnits =
      struct
        type units = token Unit.t list
        type 'src lexer = 'src -> (units, units * message) Stdlib.result

        let scan_all_units = function
          Stdlib.Error msg ->
            flush_all (); Stdlib.Error ([], msg)
        | Ok Core.{read_units; lexbuf; close; _} ->
            let close_all () = flush_all (); close ()
            and result = read_units lexbuf
            in close_all (); result

        let from_lexbuf   lexbuf = from_lexbuf   lexbuf |> scan_all_units
        let from_channel channel = from_channel channel |> scan_all_units
        let from_string   string = from_string   string |> scan_all_units
        let from_buffer   buffer = from_buffer   buffer |> scan_all_units
        let from_file        src = from_file        src |> scan_all_units
      end
  end
