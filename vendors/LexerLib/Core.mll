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
    type 'token lex_unit

    (* Utility types *)

    type file_path = string
    type message   = string Region.reg

    (* LEXER INSTANCE (see README.md) *)

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type 'token units  = 'token lex_unit list

    type 'token error = {
      used_units : 'token units;
      message    : message
    }

    type 'token instance = {
      input      : input;
      read_units : Lexing.lexbuf -> ('token units, 'token error) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit
    }

    val open_stream : input -> ('token instance, message) result
  end

(* THE FUNCTOR *)

module Make (Config : Preprocessor.Config.S) (Client : Client.S) =
  struct
    module State   = Client.State
    module Unit    = State.Unit
    module Options = Unit.Options

    type 'token lex_unit = 'token Unit.t

    type 'token units = 'token lex_unit list

    (* Errors (NOT EXPORTED) *)

    type message = string Region.reg

    type 'token error = {
      used_units : 'token units;
      message    : message
    }

    (* Failure *)

    let fail state region error =
      let value      = Error.to_string error in
      let message    = Region.{value; region} in
      let used_units = List.rev state#lexical_units
      in Error {used_units; message}

    (* Internal client *)

    module Client =
      struct
        let mk_string = Client.mk_string

        let callback state lexbuf =
          let () = Lexbuf.rollback lexbuf in
          match Client.callback state lexbuf with
            Ok (token, state) ->
              Ok (state#push_token token)
          | Error message ->
              let used_units = List.rev state#lexical_units
              in Error {used_units; message}
      end

    let callback_with_cont scan state lexbuf =
      match Client.callback state lexbuf with
        Ok state -> scan state lexbuf
      | Error _ as err -> err

    (* The lexer instance: the main exported data type *)

    type file_path = string

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type 'token instance = {
      input      : input;
      read_units : Lexing.lexbuf -> ('token units, 'token error) result;
      lexbuf     : Lexing.lexbuf;
      close      : unit -> unit
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
    | File path ->
        try
          let channel  = open_in path in
          let close () = close_in channel in
          let lexbuf   = Lexing.from_channel channel in
          let ()       = Lexbuf.reset ~file:path lexbuf
          in Ok (lexbuf, close)
        with Sys_error msg ->
          let region = Region.min ~file:path (* [path] may be [""] *)
          in Error Region.{region; value=msg}

    (* The main function *)

    let open_stream scan input : ('token instance, message) result =
      let read_units lexbuf =
        let state = State.empty ~file:Options.input in
        match scan state lexbuf with
          Ok state -> Ok (List.rev state#lexical_units)
        | Error _ as err -> err in
      match lexbuf_from_input input with
        Ok (lexbuf, close) ->
          Ok {read_units; input; lexbuf; close}
      | Error _ as err -> err

    (* Reading UTF-8 encoded characters *)

    let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
      let ()  = Lexbuf.rollback lexbuf in
      let len = thread#length in
      match scan_utf8 thread state lexbuf with
        Stdlib.Ok (thread, state) ->
          let delta = thread#length - len in
          let stop  = state#pos#shift_one_uchar delta
          in callback thread (state#set_pos stop) lexbuf
      | Error (thread, state, error) ->
          let delta  = thread#length - len in
          let stop   = state#pos#shift_one_uchar delta in
          let region = Region.make ~start:state#pos ~stop
          in fail state region error

    let open_block thread state =
      Stdlib.Error (thread, state, Error.Unterminated_comment)

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
        (match in_string thread state lexbuf with
           Ok (thread, state) ->
             let token = Client.mk_string thread
             in scan (state#push_token token) lexbuf
         | Error _ as err -> err)
    | Some _ | None -> callback_with_cont scan state lexbuf }

  (* Comments *)

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.block with
      Some block when block#opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = Thread.make ~opening:region in
        let thread        = thread#push_string lexeme in
        (match in_block block thread state lexbuf with
           Ok (thread, state) ->
             scan (state#push_block thread) lexbuf
         | Error _ as err -> err)
    | Some _ | None -> callback_with_cont scan state lexbuf }

| line_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.line with
      Some opening when opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread        = Thread.make ~opening:region in
        let thread        = thread#push_string lexeme in
        (match in_line thread state lexbuf with
           Ok (thread, state) ->
             scan (state#push_line thread) lexbuf
         | Error _ as err -> err)
    | Some _ | None -> callback_with_cont scan state lexbuf }

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* (nl | eof) {
    let pos'    = state#pos in
    (* TODO *)
    let State.{state; region; _} = state#sync lexbuf in
    let flag    = match flag with
                    Some '1' -> Some Directive.Push
                  | Some '2' -> Some Directive.Pop
                  | _        -> None in
    let linenum = int_of_string line in
    let value   = linenum, file, flag in
    let dir     = Directive.Linemarker Region.{value; region} in
    let state   = state#push_directive dir in
    (* TODO *)
    match flag with
      Some Directive.Pop  -> Ok state
    | Some Directive.Push ->
        let pos   = region#start#add_nl in
        let pos   = (pos#set_file file)#set_line linenum in
        let pos   = pos#reset_cnum in
        let state = state#set_pos pos in
        (match scan state lexbuf with
           Ok state -> scan (state#set_pos pos') lexbuf
         | Error _ as err -> err)
    | None -> (* This is the first linemarker *)
        let pos   = region#start#add_nl in
        let pos   = (pos#set_file file)#set_line linenum in
        let state = state#set_pos pos
        in scan state lexbuf }

  (* End-of-File: we return the final state *)

| eof { Client.callback state lexbuf }

  (* Other tokens *)

| _ { callback_with_cont scan state lexbuf }


(* Block comments *)

and in_block block thread state = parse
  string_delimiter {
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when delimiter = lexeme ->
        let opening = thread#opening in
        let State.{region; state; _} = state#sync lexbuf in
        let thread = thread#push_string lexeme in
        let thread = thread#set_opening region in
        (match in_string thread state lexbuf with
           Ok (thread, state) ->
             let thread = thread#push_string lexeme in
             let thread = thread#set_opening opening
             in in_block block thread state lexbuf
         | Error _ as err -> err)
    | Some _ | None ->
        scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                       thread state lexbuf }

| block_comment_opening {
    let lexeme = Lexing.lexeme lexbuf in
    if   block#opening = lexeme
    then let opening = thread#opening in
         let State.{region; state; _} = state#sync lexbuf in
         let thread = thread#push_string lexeme in
         let thread = thread#set_opening region in
         (match in_block block thread state lexbuf with
            Ok (thread, state) ->
              let thread = thread#set_opening opening
              in in_block block thread state lexbuf
          | Error _ as err -> err)
    else scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                        thread state lexbuf }

| block_comment_closing {
    let State.{state; lexeme; _} = state#sync lexbuf in
    if   block#closing = lexeme
    then Ok (thread#push_string lexeme, state)
    else scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                        thread state lexbuf }
| nl as nl {
    let thread = thread#push_string nl
    and state  = state#newline lexbuf
    in in_block block thread state lexbuf }

| eof { fail state thread#opening Error.Unterminated_comment }

| _ { scan_utf8_wrap (scan_utf8 open_block) (in_block block)
                     thread state lexbuf }

(* Line comments *)

and in_line thread state = parse
  nl as nl { Ok (thread#push_string nl, state#newline lexbuf) }
| eof      { Ok (thread, state) }
| _        { let scan_utf8 =
               scan_utf8 (fun thread state -> Ok (thread, state))
             in scan_utf8_wrap scan_utf8 in_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8 if_eof thread state = parse
  eof { if_eof thread state }
| _   { let lexeme = Lexing.lexeme lexbuf in
        let thread = thread#push_string lexeme in
        let () = state#supply (Bytes.of_string lexeme) 0 1 in
        match Uutf.decode state#decoder with
          `Uchar _     -> Ok (thread, state)
        | `Malformed _
        | `End         -> Error (thread, state, Error.Invalid_utf8_sequence)
        | `Await       -> scan_utf8 if_eof thread state lexbuf }

(* Scanning strings *)

and in_string thread state = parse
  string_delimiter {
         let State.{state; lexeme; region} = state#sync lexbuf in
         match Config.string with
           Some delimiter when delimiter = lexeme ->
             (* Closing the string *)
             Ok (thread#set_closing region, state)
         | Some _ | None -> (* Still inside the string *)
             let thread = thread#push_string lexeme
             in in_string thread state lexbuf                     }
| '\\' { let State.{state; _} = state#sync lexbuf
         in unescape thread state lexbuf                          }
| nl   { fail state thread#opening Error.Newline_in_string        }
| eof  { fail state thread#opening Error.Unterminated_string      }
| ['\000' - '\031'] as c  (* Control characters *)
       { let State.{region; _} = state#sync lexbuf in
         fail state region (Error.Invalid_character_in_string c)  }
| _    { let State.{state; lexeme; _} = state#sync lexbuf in
         in_string (thread#push_string lexeme) state lexbuf       }

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

    let open_stream : input -> ('token instance, message) result =
      let first_call = ref true in
      let scan state =
        (if !first_call then (first_call := false; init) else scan) state
      in open_stream scan

  end (* of functor [Make] *)
(* END TRAILER *)
}
