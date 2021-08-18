(* This module is a wrapper for running the LIGO parsers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module PreprocMainGen = Preprocessor.PreprocMainGen

module type CONFIG  = Preprocessor.Config.S
module type OPTIONS = ParserLib.Options.S
module type PARSER  = ParserLib.API.PARSER

(* Internal dependencies *)

module type TOKEN       = Lexing_shared.Token.S
module type SELF_TOKENS = Lexing_shared.Self_tokens.S

module LexerMainGen = Lexing_shared.LexerMainGen

(* The functor *)

module type PRINTER =
  sig
    type tree
    type state

    val mk_state :
      offsets:bool -> mode:[`Point|`Byte] -> buffer:Buffer.t -> state

    val print_tokens : state -> tree -> unit
    val pp_cst       : state -> tree -> unit
  end

module type PRETTY =
  sig
    type tree
    val print : tree -> PPrint.document
  end

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

module Make
         (Config      : CONFIG)
         (Options     : OPTIONS)
         (Token       : TOKEN)
         (ParErr      : sig val message : int -> string end)
         (Self_tokens : SELF_TOKENS with type token = Token.t)
         (CST         : sig type t end)
         (Parser      : PARSER with type token = Token.t
                                and type tree = CST.t)
         (Printer     : PRINTER with type tree = CST.t)
         (Pretty      : PRETTY with type tree = CST.t)
 =
  struct
    (* Reading the preprocessor CLI *)

    module PreprocParams = Preprocessor.CLI.Make (Config)

    (* Reading the lexer CLI *)

    module LexerParams = LexerLib.CLI.Make (PreprocParams)

    (* Reading the parser CLI *)

    module Parameters = ParserLib.CLI.Make (LexerParams)

    (* Instantiating the main lexer *)

    module MainLexer =
      LexerMainGen.Make (Config) (Options) (Token) (Self_tokens)

    (* All exits *)

    let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

    let red_exit msg = print_in_red msg; exit 1

    let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

    let print_and_quit msg = print_string msg; flush stdout; exit 0

    (* Checking for errors and valid exits *)

    let check_cli () =
      MainLexer.check_cli ();
      match Parameters.Status.status with
        `SyntaxError  msg
      | `WrongFileExt msg
      | `FileNotFound msg -> cli_error msg
      | `Help         buf
      | `CLI          buf -> print_and_quit (Buffer.contents buf)
      | `Version      ver -> print_and_quit (ver ^ "\n")
      | `Conflict (o1,o2) ->
           cli_error (Printf.sprintf "Choose either %s or %s." o1 o2)
      | `Done -> ()

    (* Main *)

    module MainParser = ParserLib.API.Make (MainLexer) (Parser)

    let wrap : (Parser.tree, MainParser.message) result -> unit =
      function
        Stdlib.Ok tree ->
          if Options.pretty then
            let doc = Pretty.print tree in
            let width =
              match Terminal_size.get_columns () with
                None -> 60
              | Some c -> c in
            begin
              PPrint.ToChannel.pretty 1.0 width stdout doc;
              print_newline ()
            end
          else
            let buffer = Buffer.create 231 in
            let state  = Printer.mk_state
                           ~offsets:Options.offsets
                           ~mode:Options.mode
                           ~buffer in
            if Options.cst then
              begin
                Printer.pp_cst state tree;
                Printf.printf "%s%!" (Buffer.contents buffer)
              end
            else
              if Options.cst_tokens then
                begin
                  Printer.print_tokens state tree;
                  Printf.printf "%s%!" (Buffer.contents buffer);
                end
              else ();
            flush_all ()
      | Error Region.{value; region} ->
         let reg = region#to_string ~file:true ~offsets:true `Point in
         let msg = Printf.sprintf "Parse error %s:\n%s" reg value
         in (flush_all (); print_in_red msg)

    (* Instantiating the preprocessor *)

    module Preproc = Preprocessor.PreprocMainGen.Make (PreprocParams)

    let parse () =
      if Options.preprocess then
        match Preproc.preprocess () with
          Stdlib.Error _ -> ()
        | Stdlib.Ok (buffer, _deps) ->
            if Options.show_pp then
              Printf.printf "%s%!" (Buffer.contents buffer)
            else ();
            let string = Buffer.contents buffer in
            let lexbuf = Lexing.from_string string in
            let open MainParser in
            if Options.mono then
              mono_from_lexbuf lexbuf |> wrap
            else
              incr_from_lexbuf (module ParErr) lexbuf |> wrap
      else
        let open MainParser in
        match Options.input with
          None ->
            if Options.mono then
              mono_from_channel stdin |> wrap
            else
              incr_from_channel (module ParErr) stdin |> wrap
        | Some file_path ->
            if Options.mono then
              mono_from_file file_path |> wrap
            else
              incr_from_file (module ParErr) file_path |> wrap
  end
