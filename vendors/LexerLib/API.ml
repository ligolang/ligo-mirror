(* Vendor dependencies *)

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* The functor itself *)

module type S =
  sig
    type token

    type file_path = string
    type message   = string Region.reg

    type ('src,'dst) lexer = 'src -> ('dst, message) Stdlib.result

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
        type nonrec 'src lexer = ('src, token Unit.t list) lexer

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

    type ('src,'dst) lexer = 'src -> ('dst, message) Stdlib.result

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
        let scan_all_tokens = function
          Stdlib.Error _ as err -> flush_all (); err
        | Ok Core.{read_token; lexbuf; close; _} ->
            let close_all () = flush_all (); close () in
            let rec read_tokens tokens =
              match read_token lexbuf with
                Stdlib.Ok token ->
                  if   Token.is_eof token
                  then Stdlib.Ok (List.rev tokens)
                  else read_tokens (token::tokens)
              | Error _ as err -> err in
            let result = read_tokens []
            in close_all (); result

        let from_lexbuf   lexbuf = from_lexbuf   lexbuf |> scan_all_tokens
        let from_channel channel = from_channel channel |> scan_all_tokens
        let from_string   string = from_string   string |> scan_all_tokens
        let from_buffer   buffer = from_buffer   buffer |> scan_all_tokens
        let from_file        src = from_file        src |> scan_all_tokens
      end

    module LexUnits =
      struct
        type nonrec 'src lexer = ('src, token Unit.t list) lexer

        let scan_all_units = function
          Stdlib.Error _ as err -> flush_all (); err
        | Ok Core.{read_unit; lexbuf; close; _} ->
            let close_all () = flush_all (); close () in
            let rec read_units units =
              match read_unit lexbuf with
                Stdlib.Ok (`Token token as unit) ->
                  if   Token.is_eof token
                  then Stdlib.Ok (List.rev units)
                  else read_units (unit::units)
              | Ok unit -> read_units (unit::units)
              | Error _ as err -> err in
            let result = read_units []
            in close_all (); result

        let from_lexbuf   lexbuf = from_lexbuf   lexbuf |> scan_all_units
        let from_channel channel = from_channel channel |> scan_all_units
        let from_string   string = from_string   string |> scan_all_units
        let from_buffer   buffer = from_buffer   buffer |> scan_all_units
        let from_file        src = from_file        src |> scan_all_units
      end
  end
