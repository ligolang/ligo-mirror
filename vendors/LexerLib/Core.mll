(* A library for writing UTF8-aware lexers *)

{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module Utils  = Simple_utils.Utils

let (<@) = Utils.(<@)

(* Wrapping tokens, markup and directives. See module [Unit]. *)

let mk_token     (token,     state) = `Token     token,     state
let mk_markup    (markup,    state) = `Markup    markup,    state
let mk_directive (directive, state) = `Directive directive, state

(* LEXER ENGINE *)

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf) in
  let pos_cnum = lexbuf.lex_curr_p.pos_cnum - len in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum}

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line lexbuf] modifies in-place the lexing
   buffer [lexbuf] so the lexing engine records that the file
   associated with [lexbuf] is named [file], and the current line is
   [line]. *)

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
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum = bol + offset}

let reset ?file ?(line=1) ?offset lexbuf =
  let () =
    match file with
      Some file -> reset_file file lexbuf
    |      None -> () in
  let () = reset_line line lexbuf in
  match offset with
    Some offset -> reset_offset offset lexbuf
  |        None -> ()

(* LEXER INSTANCE *)

(* Pretty-printing a lexical unit *)

let output_unit config out_channel lex_unit =
  let output    str = Printf.fprintf out_channel "%s%!" str in
  let output_nl str = output (str ^ "\n")
  and offsets = config#offsets
  and mode    = config#mode in
  match config#command with
    Some `Tokens ->
      (match lex_unit with
         `Token token ->
           config#to_string ~offsets mode token |> output_nl
       | `Markup _ | `Directive _ -> ()) (* Only tokens *)
  | Some `Copy ->
     let lexeme =
       match lex_unit with
         `Token token -> config#to_lexeme token
       | `Markup m    -> Markup.to_lexeme m
       | `Directive d -> Directive.to_lexeme d
     in output lexeme
  | Some `Units ->
      let string =
        match lex_unit with
          `Token token -> config#to_string ~offsets mode token
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

type 'token instance = {
  input      : input;
  read_token : Lexing.lexbuf -> ('token, message) result;
  read_unit  : Lexing.lexbuf -> ('token Unit.t, message) result;
  lexbuf     : Lexing.lexbuf;
  close      : unit -> unit;
  window     : unit -> 'token State.window option
}

let lexbuf_from_input config = function
  String s ->
    Ok (Lexing.from_string s, fun () -> ())
| Channel chan ->
    let close () = close_in chan in
    Ok (Lexing.from_channel chan, close)
| Buffer b ->
    let () =
      match config#input with
        None | Some "" -> ()
      | Some path -> reset ~file:path b
    in Ok (b, fun () -> ())
| File "" ->
    Stdlib.Error (Region.wrap_ghost "File not found.")
| File path ->
    try
      let channel  = open_in path in
      let close () = close_in channel in
      let lexbuf   = Lexing.from_channel channel in
      let ()       = reset ~file:path lexbuf
      in Ok (lexbuf, close)
    with Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)

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

(* The main function *)

type 'token scanner =
  'token State.t ->
  Lexing.lexbuf ->
  ('token Unit.t * 'token State.t, message) Stdlib.result

let open_stream :
  'token scanner ->
  'token State.config ->
  input ->
  ('token instance, message) Stdlib.result =
  fun scan config input ->
  let log       = output_unit config stdout
  and scan      = drop <@ scan
  and file_path = match config#input with
                    Some path -> path
                  | _ -> ""
  and   decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let    supply = Uutf.Manual.src decoder in
  let     state = ref (State.make
                         ~config
                         ~window:None
                         ~pos:(Pos.min ~file:file_path)
                         ~decoder
                         ~supply) in
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

  match lexbuf_from_input config input with
    Stdlib.Ok (lexbuf, close) ->
      let read_unit  = lift read_unit
      and read_token = lift read_token in
      Ok {read_unit; read_token; input; lexbuf; close; window}
  | Error _ as e -> e

(* Reading UTF-8 encoded characters *)

let scan_utf8_wrap scan_utf8 callback thread state lexbuf =
  let ()             = rollback lexbuf in
  let len            = thread#length in
  let thread, status = scan_utf8 thread state lexbuf in
  let delta          = thread#length - len in
  let stop           = state#pos#shift_one_uchar delta in
  match status with
    Ok () -> callback thread (state#set_pos stop) lexbuf
  | Stdlib.Error error ->
      let region = Region.make ~start:state#pos ~stop
      in fail region error

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
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let esc        = "\\n" | "\\\"" | "\\\\" | "\\b"
               | "\\r" | "\\t" | "\\x" byte
let flag       = '1' | '2' (* Linemarkers *)

(* Comment delimiters *)

let pascaligo_block_comment_opening  = "(*"
let pascaligo_block_comment_closing  = "*)"
let pascaligo_line_comment           = "//"

let cameligo_block_comment_opening   = "(*"
let cameligo_block_comment_closing   = "*)"
let cameligo_line_comment            = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment          = "//"

let michelson_block_comment_opening = "/*"
let michelson_block_comment_closing = "*/"
let michelson_line_comment          = "#"

let jsligo_block_comment_opening    = "/*"
let jsligo_block_comment_closing    = "*/"
let jsligo_line_comment             = "//"

let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening
| jsligo_block_comment_opening

let block_comment_closings =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing
| michelson_block_comment_closing
| jsligo_block_comment_opening

let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment
| jsligo_line_comment

(* RULES (SCANNERS) *)

rule scan client state = parse
  (* Markup *)

  nl    { state#mk_newline lexbuf |> mk_markup }
| ' '+  { state#mk_space   lexbuf |> mk_markup }
| '\t'+ { state#mk_tabs    lexbuf |> mk_markup }

  (* Strings *)

| '\''
| '"' as lexeme {
    if client#support_string_delimiter lexeme then
      let State.{region; state; _} = state#sync lexbuf in
      let thread             = Thread.make region in
      scan_string lexeme thread state lexbuf |> client#mk_string
    else (rollback lexbuf; client#callback state lexbuf) }

  (* Comment *)

| block_comment_openings as lexeme {
    match state#config#block with
      Some block when block#opening = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread             = Thread.make region in
        let thread             = thread#push_string lexeme in
        let thread, state      = scan_block block thread state lexbuf
        in state#mk_block thread |> mk_markup
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }

| line_comments as lexeme {
    match state#config#line with
      Some line when line = lexeme ->
        let State.{region; state; _} = state#sync lexbuf in
        let thread             = Thread.make region in
        let thread             = thread#push_string lexeme in
        let thread, state      = scan_line thread state lexbuf
        in state#mk_line thread |> mk_markup
    | Some _ | None -> (* Not a comment for this syntax *)
        rollback lexbuf; client#callback state lexbuf }

  (* Linemarkers preprocessing directives (from #include) *)

| '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ (('1' | '2') as flag))? blank* (nl | eof) {
    state#mk_linemarker ~line ~file ?flag lexbuf
    |> mk_directive }

  (* Other tokens *)

| eof | _ { rollback lexbuf;
            client#callback state lexbuf (* May raise exceptions *) }

(* Block comments

   (For Emacs: ("(*") The lexing of block comments must take care of
   embedded block comments that may occur within, as well as strings,
   so no substring "*/" or "*)" may inadvertently close the
   block. This is the purpose of the first case of the scanner
   [scan_block]. *)

and scan_block block thread state = parse
  block_comment_openings as lexeme {
    if   block#opening = lexeme
    then let opening            = thread#opening in
         let State.{region; state; _} = state#sync lexbuf in
         let thread             = thread#push_string lexeme in
         let thread             = thread#set_opening region in
         let scan_next          = scan_block block in
         let thread, state      = scan_next thread state lexbuf in
         let thread             = thread#set_opening opening
         in scan_block block thread state lexbuf
    else begin
           rollback lexbuf;
           scan_char_in_block block thread state lexbuf
         end }

| block_comment_closings as lexeme {
    if   block#closing = lexeme
    then thread#push_string lexeme, (state#sync lexbuf).state
    else begin
           rollback lexbuf;
           scan_char_in_block block thread state lexbuf
         end }

| nl as nl {
    let ()     = Lexing.new_line lexbuf
    and state  = state#set_pos (state#pos#new_line nl)
    and thread = thread#push_string nl in
    scan_block block thread state lexbuf }

| eof { let err = Error.Unterminated_comment block#closing
        in fail thread#opening err }

| _ { rollback lexbuf;
      scan_char_in_block block thread state lexbuf }

and scan_char_in_block block thread state = parse
  _ { let if_eof thread =
        let err = Error.Unterminated_comment block#closing
        in fail thread#opening err in
      let scan_utf8 = scan_utf8_char if_eof
      and callback  = scan_block block in
      scan_utf8_wrap scan_utf8 callback thread state lexbuf }

(* Line comments *)

and scan_line thread state = parse
  nl as nl { let ()     = Lexing.new_line lexbuf
             and thread = thread#push_string nl
             and state  = state#set_pos (state#pos#new_line nl)
             in thread, state }
| eof      { thread, state }
| _        { let scan_utf8 = scan_utf8_char (fun _ -> Stdlib.Ok ())
             in scan_utf8_wrap scan_utf8 scan_line thread state lexbuf }

(* Scanning UTF-8 encoded characters *)

and scan_utf8_char if_eof thread state = parse
     eof { thread, if_eof thread }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _
           | `End         -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8_char if_eof thread state lexbuf }

(* Scanning strings *)

and scan_string delimiter thread state = parse
  nl     { fail thread#opening Error.Broken_string }
| eof    { fail thread#opening Error.Unterminated_string }
| ['\t' '\r' '\b']
         { let State.{region; _} = state#sync lexbuf
           in fail region Error.Invalid_character_in_string }
| '"'    {
  if delimiter = '"' then
    let State.{state; _} = state#sync lexbuf
    in thread, state
  else
    let State.{state; _} = state#sync lexbuf in
    scan_string delimiter (thread#push_char '"') state lexbuf
  }
| '\''   {
  if delimiter = '\'' then
    let State.{state; _} = state#sync lexbuf
    in thread, state
  else
    let State.{state; _} = state#sync lexbuf in
    scan_string delimiter (thread#push_char '\'') state lexbuf

}
| esc    { let State.{lexeme; state; _} = state#sync lexbuf in
           let thread = thread#push_string lexeme
           in scan_string delimiter thread state lexbuf }
| '\\' _ { let State.{region; _} = state#sync lexbuf
           in fail region Error.Undefined_escape_sequence }
| _ as c { let State.{state; _} = state#sync lexbuf in
           scan_string delimiter (thread#push_char c) state lexbuf }

  (* Scanner called first *)

and init client state = parse
  utf8_bom { state#mk_bom lexbuf |> mk_markup           }
| _        { rollback lexbuf; scan client state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let mk_scan (client: 'token Client.t) : 'token scanner =
  let internal_client =
    object
      method mk_string = mk_token <@ client#mk_string

      method callback state lexbuf =
        mk_token (drop (client#callback state) lexbuf)

      method support_string_delimiter = client#support_string_delimiter
    end

  and first_call = ref true in

  fun state ->
    let scanner =
      if !first_call then (first_call := false; init) else scan
    in lift (scanner internal_client state)

let open_stream client = open_stream @@ mk_scan client

(* END TRAILER *)
}
