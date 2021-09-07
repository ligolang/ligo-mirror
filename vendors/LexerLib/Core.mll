(* A library for writing UTF8-aware lexers *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Lexbuf = Simple_utils.Lexbuf

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

    type tokens = token list
    type units  = token Unit.t list

    type instance = {
      input       : input;
      read_tokens : Lexing.lexbuf -> (tokens, tokens * message) result;
      read_units  : Lexing.lexbuf -> (units, units * message) result;
      lexbuf      : Lexing.lexbuf;
      close       : unit -> unit
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

    type message = string Region.reg
    type error = token Unit.t list * message

    exception Error of error

    (* Encoding a function call in exception-raising style (ERS) to
       error-passing style (EPS) *)

    let filter_token = function
        `Token token -> Some token
    | `Markup _ | `Directive _ -> None

    let lift_units scanner lexbuf =
      try Stdlib.Ok (scanner lexbuf) with
        Error err -> Stdlib.Error err

    let lift_tokens scanner lexbuf =
      try Stdlib.Ok (scanner lexbuf) with
        Error (units, msg) ->
          let tokens = List.filter_map filter_token units
          in Stdlib.Error (tokens, msg)

    (* Failure *)

    let fail state region error =
      let value = Error.to_string error in
      let msg   = Region.{value; region} in
      let units = List.rev state#lexical_units
      in raise (Error (units, msg))

    (* Internal client *)

    module Client =
      struct
        let mk_string = Client.mk_string

        let callback state lexbuf =
          let () = Lexbuf.rollback lexbuf in
          let token, state =
            match Client.callback state lexbuf with
              Stdlib.Ok ok -> ok
            | Stdlib.Error msg ->
                let units = List.rev state#lexical_units
                in raise (Error (units, msg))
          in state#push_token token
      end

    (* Printing a lexical unit *)

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

    let output_units out_channel lex_units =
      List.iter (output_unit out_channel) lex_units

    (* The lexer instance: the main exported data type *)

    type file_path = string

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type tokens = token list
    type units  = token Unit.t list

    type instance = {
      input       : input;
      read_tokens : Lexing.lexbuf -> (tokens, tokens * message) result;
      read_units  : Lexing.lexbuf -> (units, units * message) result;
      lexbuf      : Lexing.lexbuf;
      close       : unit -> unit
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
      let read_units lexbuf =
        let file_path =
          match Options.input with
            Some path -> path
          | _ -> "" in
        let state = State.empty ~file:file_path in
        let units = scan state lexbuf |> List.rev in
        let ()    = output_units stdout units
        in units in

      let read_tokens lexbuf =
        List.filter_map filter_token (read_units lexbuf) in

      match lexbuf_from_input input with
        Stdlib.Ok (lexbuf, close) ->
          let read_units  = lift_units read_units
          and read_tokens = lift_tokens read_tokens in
          Ok {read_units; read_tokens; input; lexbuf; close}
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
          in fail state region error

    (* Scanning arbitrary characters in a block comment *)

    let scan_chars scan_utf8_char in_block thread state lexbuf =
      let if_eof thread =
        fail state thread#opening Error.Unterminated_comment in
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
|   cameligo_block_comment_opening
| reasonligo_block_comment_opening
|  michelson_block_comment_opening
|     jsligo_block_comment_opening

let block_comment_closing =
   pascaligo_block_comment_closing
|   cameligo_block_comment_closing
| reasonligo_block_comment_closing
|  michelson_block_comment_closing
|     jsligo_block_comment_closing

let line_comment_opening =
   pascaligo_line_comment_opening
|   cameligo_line_comment_opening
| reasonligo_line_comment_opening
|  michelson_line_comment_opening
|     jsligo_line_comment_opening

(* String delimiters *)

let  pascaligo_string_delimiter = "\""
let   cameligo_string_delimiter = "\""
let reasonligo_string_delimiter = "\""
let  michelson_string_delimiter = "\""
let     jsligo_string_delimiter = "\""

let string_delimiter =
   pascaligo_string_delimiter
|   cameligo_string_delimiter
| reasonligo_string_delimiter
|  michelson_string_delimiter
|     jsligo_string_delimiter

(* RULES (SCANNERS) *)

rule scan state = parse
  (* Markup *)

  nl    { scan (state#push_newline lexbuf) lexbuf }
| ' '+  { scan (state#push_space   lexbuf) lexbuf }
| '\t'+ { scan (state#push_tabs    lexbuf) lexbuf }

  (* Strings *)

| string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when delimiter = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread = Thread.make ~opening:region in
        let thread, state = in_string thread state lexbuf in
        let token = Client.mk_string thread
        in scan (state#push_token token) lexbuf
    | Some _ | None ->
        scan (Client.callback state lexbuf) lexbuf }

  (* Comments *)

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.block with
      Some block when block#opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = Thread.make ~opening:region in
        let thread        = thread#push_string lexeme in
        let thread, state = in_block block thread state lexbuf
        in scan (state#push_block thread) lexbuf
    | Some _ | None ->
        scan (Client.callback state lexbuf) lexbuf }

| line_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.line with
      Some opening when opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = Thread.make ~opening:region in
        let thread        = thread#push_string lexeme in
        let thread, state = in_line thread state lexbuf
        in scan (state#push_line thread) lexbuf
    | Some _ | None ->
        scan (Client.callback state lexbuf) lexbuf }

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* (nl | eof) {
    (* TODO: Recursive call like in Preprocessor with #include *)
    let state = state#push_linemarker ~line ~file ?flag lexbuf
    in scan state lexbuf }

  (* Other tokens *)

| eof | _ { scan (Client.callback state lexbuf) lexbuf }

(* Block comments *)

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

| eof { fail state thread#opening Error.Unterminated_comment }

| _ { scan_chars scan_utf8_char (in_block block) thread state lexbuf }

(* Line comments *)

and in_line thread state = parse
  nl as nl { thread#push_string nl, state#newline lexbuf }
| eof      { thread, state }
| _        { let scan_utf8 = scan_utf8_char (fun _ -> Stdlib.Ok ())
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
             in_string thread state lexbuf                   }
| '\\' { let State.{state; _} = state#sync lexbuf
         in unescape thread state lexbuf                     }
| nl   { fail state thread#opening Error.Newline_in_string         }
| eof  { fail state thread#opening Error.Unterminated_string       }
| ['\000' - '\031'] as c  (* Control characters *)
       { let State.{region; _} = state#sync lexbuf in
         fail state region (Error.Invalid_character_in_string c)   }
| _    { let State.{state; lexeme; _} = state#sync lexbuf in
         in_string (thread#push_string lexeme) state lexbuf  }

and unescape thread state = parse
  string_delimiter {
         let State.{state; lexeme; _} = state#sync lexbuf in
         let interpretation =
           match Config.string with
             Some delimiter when delimiter = lexeme ->
               lexeme (* E.g. unescaped \" into " *)
           | Some _ | None -> "\\" ^ lexeme (* verbatim *) in
         let thread = thread#push_string interpretation
         in in_string thread state lexbuf }
| 'n'  { let State.{state; _} = state#sync lexbuf
         and thread = thread#push_char '\n' (* Unescaped "\n" into '\010' *)
         in in_string thread state lexbuf }
| '\\' { let State.{state; lexeme; _} = state#sync lexbuf in
         let thread = thread#push_string lexeme (* Unescaped "\\" into '\\' *)
         in in_string thread state lexbuf }
| _    { Lexbuf.rollback lexbuf; (* Not a valid escape sequence *)
         let thread = thread#push_char '\\' (* verbatim *)
         in in_string thread state lexbuf }

(* Scanner called first *)

and init state = parse
  utf8_bom { scan (state#push_bom lexbuf) lexbuf       }
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
