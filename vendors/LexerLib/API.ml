(* Vendor dependencies *)

module Region = Simple_utils.Region
module Lexbuf = Simple_utils.Lexbuf

(* The functor itself *)

module type S =
  sig
    type lex_unit

    type units = lex_unit list

    type error = {
      used_units : units;
      message    : string Region.reg
    }

    type 'src lexer = 'src -> (units, error) result

    val from_lexbuf  : file:string -> Lexing.lexbuf lexer
    val from_channel : file:string -> in_channel lexer
    val from_string  : file:string -> string lexer
    val from_file    : string lexer
    val from_buffer  : Buffer.t lexer
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S) =
  struct
    module Core = Core.Make (Config) (Client)

    type lex_unit = Core.lex_unit

    type units = Core.units

    type error = Core.error = {
      used_units : units;
      message    : string Region.reg
    }

    type 'src lexer = 'src -> (units, error) result

    (* Generic lexer for all kinds of inputs *)

    let generic ~file lexbuf_of source =
      let lexbuf = lexbuf_of source in
      let () = if file <> "" then Lexbuf.reset ~file lexbuf
      in Core.(open_stream @@ Lexbuf (file, lexbuf))
    (* Getting lexer instances for various inputs *)

    let inst_from_lexbuf  ~file = generic ~file (fun x -> x)
    let inst_from_channel ~file = generic ~file Lexing.from_channel
    let inst_from_string  ~file = generic ~file Lexing.from_string

    let inst_from_buffer buffer =
      inst_from_string ~file:"" @@ Buffer.contents buffer
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

    let from_lexbuf  ~file  lexbuf = inst_from_lexbuf  ~file  lexbuf |> scan_all_units
    let from_channel ~file channel = inst_from_channel ~file channel |> scan_all_units
    let from_string  ~file  string = inst_from_string  ~file  string |> scan_all_units
    let from_buffer         buffer = inst_from_buffer         buffer |> scan_all_units
    let from_file              src = inst_from_file              src |> scan_all_units
  end
