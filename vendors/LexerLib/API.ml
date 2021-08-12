(* Vendor dependencies *)

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

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

(* General configuration *)

module type CONFIG = module type of Preprocessor.Config

(* CLI options *)

module type OPTIONS = module type of Options

(* The signature of tokens *)

module type TOKEN = module type of Token

(* The functor definition *)

module Make (Config  : CONFIG)
            (Options : OPTIONS)
            (Token   : TOKEN)
            (Client  : CLIENT with type token = Token.t) =
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

(* LEXER ENGINE *)

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line lexbuf] modifies in-place the lexing
   buffer [lexbuf] so the lexing engine records that the file
   associated with [lexbuf] is named [file], and the current line is
   [line]. *)

type file_path = string

let reset_file file lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file}

let reset_line line lexbuf =
  assert (line >= 0);
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = line}

let reset_offset offset lexbuf =
  assert (offset >= 0);
  let open Lexing in
  let bol = lexbuf.lex_curr_p.pos_bol in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum = bol + offset }

let reset ?file ?(line=1) ?offset lexbuf =
  let () =
    match file with
      Some file -> reset_file file lexbuf
    |      None -> () in
  let () = reset_line line lexbuf in
  match offset with
    Some offset -> reset_offset offset lexbuf
  |        None -> ()
