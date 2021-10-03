(* Vendor dependencies *)

module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils

(* The functor itself *)

module type S =
  sig
    type 'token lex_unit

    type file_path = string
    type message   = string Region.reg

    type 'token units = 'token lex_unit list

    type 'token error = {
      used_units : 'token units;
      message    : message
    }

    type ('token, 'src) lexer =
      'src -> ('token units, 'token error) result

    val from_lexbuf  : ('token, Lexing.lexbuf) lexer
    val from_channel : ('token, in_channel)    lexer
    val from_string  : ('token, string)        lexer
    val from_buffer  : ('token, Buffer.t)      lexer
    val from_file    : ('token, file_path)     lexer
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S) =
  struct
    module Core = Core.Make (Config) (Client)

    type 'token lex_unit = 'token Core.lex_unit

    type 'token units = 'token Core.units

    type file_path = string
    type message   = string Region.reg

    type 'token error = 'token Core.error = {
      used_units : 'token units;
      message    : message
    }

    type ('token, 'src) lexer =
      'src -> ('token units, 'token error) result

    (* Generic lexer for all kinds of inputs *)

    let generic lexbuf_of source =
      Core.(open_stream @@ Buffer (lexbuf_of source))
    (* Getting lexer instances for various inputs *)

    let inst_from_lexbuf  lexbuf  = generic (fun x -> x) lexbuf
    let inst_from_channel channel = generic Lexing.from_channel channel
    let inst_from_string  string  = generic Lexing.from_string string

    let inst_from_buffer buffer = inst_from_string @@ Buffer.contents buffer
    let inst_from_file path = Core.(open_stream (File path))

    (* Lexing the input given a lexer instance *)

    let scan_all_units = function
      Stdlib.Error message ->
        flush_all (); Error {used_units=[]; message}
    | Ok Core.{read_units; lexbuf; close; _} ->
        let close_all () = flush_all (); close ()
        and result = read_units lexbuf
        in close_all (); result

    (* Lexing all lexical units from various sources *)

    let from_lexbuf   lexbuf = inst_from_lexbuf   lexbuf |> scan_all_units
    let from_channel channel = inst_from_channel channel |> scan_all_units
    let from_string   string = inst_from_string   string |> scan_all_units
    let from_buffer   buffer = inst_from_buffer   buffer |> scan_all_units
    let from_file        src = inst_from_file        src |> scan_all_units
  end
