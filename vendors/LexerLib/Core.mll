(* A library for writing UTF8-aware lexers *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Lexbuf = Simple_utils.Lexbuf

(* Wrapping tokens, markup and directives. See module [Unit]. *)

let mk_token     (token,     state) = `Token     token,     state
let mk_markup    (markup,    state) = `Markup    markup,    state
let mk_directive (directive, state) = `Directive directive, state

(* The functor return signature *)

module type S =
  sig
    type token

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    (* LEXER INSTANCE (see README.md) *)

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type instance = {
      input      : input;
      read_token : Lexing.lexbuf -> (token, message) result;
      read_unit  : Lexing.lexbuf -> (token Unit.t, message) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit;
      window     : unit -> token State.window option
    }

    val open_stream : input -> (instance, message) Stdlib.result
  end

(* THE FUNCTOR *)

module Make (Config  : Preprocessor.Config.S)
            (Options : Options.S)
            (Token   : Token.S)
            (Client  : Client.S with type token = Token.token) =
  struct
    type token = Token.t

    (* Errors (NOT EXPORTED) *)

    exception Error of string Region.reg

    (* Encoding a function call in exception-raising style (ERS) to
       error-passing style (EPS) *)

    let lift scanner lexbuf =
      try Stdlib.Ok (scanner lexbuf) with
        Error msg -> Stdlib.Error msg

    (* Decoding a function call in EPS to ERS *)

    let drop scanner lexbuf =
      match scanner lexbuf with
        Stdlib.Ok state -> state
      | Error msg -> raise (Error msg)

    (* Failing *)

    let fail region error =
      let value = Error.to_string error in
      raise (Error Region.{value; region})

    (* Internal client *)

    module Client =
      struct
        let mk_string = Client.mk_string

        let callback state lexbuf =
          let () = Lexbuf.rollback lexbuf
          in mk_token @@ (drop @@ Client.callback state) lexbuf
      end

    (* Pretty-printing a lexical unit *)

    let output_unit out_channel lex_unit =
      let output    str = Printf.fprintf out_channel "%s%!" str in
      let output_nl str = output (str ^ "\n")
      and offsets       = Options.offsets
      and mode          = Options.mode in
      match Options.command with
        Some `Tokens ->
          (match lex_unit with
             `Token t -> Token.to_string ~offsets mode t |> output_nl
           | `Markup _ | `Directive _ -> ()) (* Only tokens *)
      | Some `Copy ->
          let lexeme =
            match lex_unit with
              `Token t     -> Token.to_lexeme t
            | `Markup m    -> Markup.to_lexeme m
            | `Directive d -> Directive.to_lexeme d
          in output lexeme
      | Some `Units ->
          let string =
            match lex_unit with
              `Token t     -> Token.to_string ~offsets mode t
            | `Markup m    -> Markup.to_string ~offsets mode m
            | `Directive d -> Directive.to_string ~offsets mode d
          in output_nl string
      | None -> ()

    (* The lexer instance: the main exported data type *)

    type file_path = string
    type message = string Region.reg

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type instance = {
      input      : input;
      read_token : Lexing.lexbuf -> (token, message) result;
      read_unit  : Lexing.lexbuf -> (token Unit.t, message) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit;
      window     : unit -> token State.window option
    }

   (* The function [Lexbuf.reset] is useful when lexing a file that
      has been previously preprocessed, in which case the argument
      [file] is the name of the file that was preprocessed, _not_ the
      preprocessed file (of which the user is not normally aware). *)

    let lexbuf_from_input = function
      String s ->
        Ok (Lexing.from_string s, fun () -> ())
    | Channel chan ->
        let close () = close_in chan in
        Ok (Lexing.from_channel chan, close)
    | Buffer b ->
        let () =
          match Options.input with
            None | Some "" -> ()
          | Some path -> Lexbuf.reset ~file:path b
        in Ok (b, fun () -> ())
    | File "" ->
        Stdlib.Error (Region.wrap_ghost "File not found.")
    | File path ->
        try
          let channel  = open_in path in
          let close () = close_in channel in
          let lexbuf   = Lexing.from_channel channel in
          let ()       = Lexbuf.reset ~file:path lexbuf
          in Ok (lexbuf, close)
        with Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)

    (* The main function *)

    let open_stream scan input =
      let log       = output_unit stdout
      and file_path = match Options.input with
                        Some path -> path
                      | _ -> "" in
      let     state = ref (State.empty ~file:file_path) in
      let window () = !state#window in

      let read_unit lexbuf =
        let unit, state' = scan !state lexbuf in
        let () = log unit in
        let () = state := state' in
        let () =
          match unit with
            `Token token -> state := !state#slide_window token
          | `Markup _ | `Directive _ -> ()
        in unit in

      let rec read_token lexbuf =
        match read_unit lexbuf with
          `Token token -> token
        | `Markup _ | `Directive _ -> read_token lexbuf in

      match lexbuf_from_input input with
        Stdlib.Ok (lexbuf, close) ->
          let read_unit  = lift read_unit
          and read_token = lift read_token in
          Ok {read_unit; read_token; input; lexbuf; close; window}
      | Error _ as e -> e

    (* Reading UTF-8 encoded characters *)

    let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
      let ()             = Lexbuf.rollback lexbuf in
      let len            = thread#length in
      let thread, status = scan_utf8 thread state lexbuf in
      let delta          = thread#length - len in
      let stop           = state#pos#shift_one_uchar delta in
      match status with
        Ok () -> callback thread (state#set_pos stop) lexbuf
      | Stdlib.Error error ->
          let region = Region.make ~start:state#pos ~stop
          in fail region error

    (* Scanning arbitrary characters in a block comment *)

    let scan_chars scan_utf8_char in_block thread state lexbuf =
      let if_eof thread =
        fail thread#opening Error.Unterminated_comment in
      let scan_utf8 = scan_utf8_char if_eof in
      scan_utf8_wrap scan_utf8 in_block thread state lexbuf

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* NAMED REGULAR EXPRESSIONS *)

let utf8_bom   = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)
let flag       = '1' | '2' (* Linemarkers *)

(* Comment delimiters *)

let pascaligo_block_comment_opening  = "(*"
let pascaligo_block_comment_closing  = "*)"
let pascaligo_line_comment_opening   = "//"

let cameligo_block_comment_opening   = "(*"
let cameligo_block_comment_closing   = "*)"
let cameligo_line_comment_opening    = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment_opening  = "//"

let michelson_block_comment_opening  = "/*"
let michelson_block_comment_closing  = "*/"
let michelson_line_comment_opening   = "#"

let jsligo_block_comment_opening     = "/*"
let jsligo_block_comment_closing     = "*/"
let jsligo_line_comment_opening      = "//"

let block_comment_opening =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening
| jsligo_block_comment_opening

let block_comment_closing =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing
| michelson_block_comment_closing
| jsligo_block_comment_opening

let line_comment_opening =
  pascaligo_line_comment_opening
| cameligo_line_comment_opening
| reasonligo_line_comment_opening
| michelson_line_comment_opening
| jsligo_line_comment_opening

(* String delimiters *)

let pascaligo_string_delimiter  = "\""
let cameligo_string_delimiter   = "\""
let reasonligo_string_delimiter = "\""
let michelson_string_delimiter  = "\""
let jsligo_string_delimiter     = "\""

let string_delimiter =
  pascaligo_string_delimiter
| cameligo_string_delimiter
| reasonligo_string_delimiter
| michelson_string_delimiter
| jsligo_string_delimiter

(* RULES (SCANNERS) *)

rule scan state = parse
  (* Markup *)

  nl    { state#mk_newline lexbuf |> mk_markup }
| ' '+  { state#mk_space   lexbuf |> mk_markup }
| '\t'+ { state#mk_tabs    lexbuf |> mk_markup }

  (* Strings *)

| string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when delimiter = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread = Thread.make ~opening:region in
        let thread, state = in_string thread state lexbuf
        in `Token (Client.mk_string thread), state
    | Some _ | None -> Client.callback state lexbuf }

  (* Comments *)

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.block with
      Some block when block#opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = Thread.make ~opening:region in
        let thread        = thread#push_string lexeme in
        let thread, state = in_block block thread state lexbuf
        in `Markup (state#mk_block thread), state
    | Some _ | None -> Client.callback state lexbuf }

| line_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.line with
      Some opening when opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = Thread.make ~opening:region in
        let thread        = thread#push_string lexeme in
        let thread, state = in_line thread state lexbuf
        in `Markup (state#mk_line thread), state
    | Some _ | None -> Client.callback state lexbuf }

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* (nl | eof) {
    state#mk_linemarker ~line ~file ?flag lexbuf
    |> mk_directive }

  (* Other tokens *)

| eof | _ { Client.callback state lexbuf }

(* Block comments

   (For Emacs: ("(*") The lexing of block comments must take care of
   embedded block comments that may occur within, as well as strings,
   so no substring "*/" or "*)" may inadvertently close the
   block. This is the purpose of the first case of the scanner
   [in_block]. *)

and in_block block thread state = parse
  string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when delimiter = lexeme ->
        let opening       = thread#opening in
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = thread#push_string lexeme in
        let thread        = thread#set_opening region in
        let thread, state = in_string thread state lexbuf in
        let thread        = thread#push_string lexeme in
        let thread        = thread#set_opening opening
        in in_block block thread state lexbuf
    | Some _ | None ->
        scan_chars scan_utf8_char (in_block block) thread state lexbuf }

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    if   block#opening = lexeme
    then let opening       = thread#opening in
         let State.{region; state; _} = state#sync lexbuf in
         let thread        = thread#push_string lexeme in
         let thread        = thread#set_opening region in
         let thread, state = in_block block thread state lexbuf in
         let thread        = thread#set_opening opening
         in in_block block thread state lexbuf
    else scan_chars scan_utf8_char (in_block block) thread state lexbuf }

| block_comment_closing {
    let State.{state; lexeme; _} = state#sync lexbuf in
    if   block#closing = lexeme
    then thread#push_string lexeme, state
    else scan_chars scan_utf8_char (in_block block) thread state lexbuf }

| nl as nl {
    let thread = thread#push_string nl
    and state  = state#newline lexbuf
    in in_block block thread state lexbuf }

| eof { fail thread#opening Error.Unterminated_comment }

| _ { scan_chars scan_utf8_char (in_block block) thread state lexbuf }


(* Line comments *)

and in_line thread state = parse
  nl as nl { thread#push_string nl, state#newline lexbuf }
| eof { thread, state }
| _   { let scan_utf8 = scan_utf8_char (fun _ -> Stdlib.Ok ())
        in scan_utf8_wrap scan_utf8 in_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8_char if_eof thread state = parse
  eof { thread, if_eof thread }
| _   { let lexeme = Lexing.lexeme lexbuf in
        let thread = thread#push_string lexeme in
        let () = state#supply (Bytes.of_string lexeme) 0 1 in
        match Uutf.decode state#decoder with
          `Uchar _     -> thread, Stdlib.Ok ()
        | `Malformed _
        | `End         -> thread, Stdlib.Error Invalid_utf8_sequence
        | `Await       -> scan_utf8_char if_eof thread state lexbuf }

(* Scanning strings *)

and in_string thread state = parse
  string_delimiter {
         let State.{state; lexeme; region} = state#sync lexbuf in
         match Config.string with
           Some delimiter when delimiter = lexeme ->
             (* Closing the string *)
             thread#set_closing region, state
         | Some _ | None -> (* Still inside the string *)
             let thread = thread#push_string lexeme in
             in_string thread state lexbuf }
| '\\' { let State.{state; _} = state#sync lexbuf
         in scan_escape thread state lexbuf            }
| nl   { fail thread#opening Error.Newline_in_string   }
| eof  { fail thread#opening Error.Unterminated_string }
| ['\000' - '\031'] as c
       { let State.{region; _} = state#sync lexbuf in
         fail region (Error.Invalid_character_in_string c)   }
| _    { let State.{state; lexeme; _} = state#sync lexbuf in
         in_string (thread#push_string lexeme) state lexbuf  }

and scan_escape thread state = parse
  string_delimiter {
         let State.{state; lexeme; _} = state#sync lexbuf in
         let escaped_delimiter =
           match Config.string with
             Some delimiter when delimiter = lexeme -> lexeme
           | Some _ | None -> "\\" ^ lexeme in
         let thread = thread#push_string escaped_delimiter
         in in_string thread state lexbuf }
| 'n'  { let State.{state; _} = state#sync lexbuf
         and thread = thread#push_char '\n'
         in in_string thread state lexbuf }
| '\\' { let State.{state; lexeme; _} = state#sync lexbuf in
         let thread = thread#push_string lexeme
         in in_string thread state lexbuf }
| _    { Lexbuf.rollback lexbuf;
         let thread = thread#push_char '\\'
         in in_string thread state lexbuf }

(* Scanner called first *)

and init state = parse
  utf8_bom { state#mk_bom lexbuf |> mk_markup           }
| _        { Lexbuf.rollback lexbuf; scan state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    let open_stream =
      let first_call = ref true in
      let scan state =
        (if !first_call then (first_call := false; init) else scan) state
      in open_stream scan

  end (* of functor [Make] *)
(* END TRAILER *)
}
