(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region

module Options   = LexerLib.Options   (* For instantiation only *)
module Unit      = LexerLib.Unit      (* For instantiation only *)
module Client    = LexerLib.Client    (* For the interface only *)
module Directive = LexerLib.Directive (* For verbatim only: TO BE REMOVED *)
module State     = LexerLib.State
module Thread    = LexerLib.Thread

(* The functorised interface *)

module Make (Options : Options.S) (Token : Token.S) =
  struct
    type token = Token.t

    (* ERRORS *)

    type error =
      Unexpected_character of char
    | Non_canonical_zero
    | Invalid_symbol of string
    | Unsupported_nat_syntax
    | Unsupported_mutez_syntax
    | Unsupported_lang_syntax
    | Invalid_natural
    | Unterminated_verbatim

    let sprintf = Printf.sprintf

    let error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Invalid_symbol s ->
        sprintf "Invalid symbol: %S.\n\
                 Hint: Check the LIGO syntax you use." s
    | Invalid_natural ->
        "Invalid natural number."
    | Unsupported_nat_syntax ->
        "Unsupported nat syntax. Please use annotations instead."
    | Unsupported_mutez_syntax ->
        "Unsupported (mu)tez syntax. Please use annotations instead."
    | Unsupported_lang_syntax ->
        "Unsupported code injection syntax."
    | Unterminated_verbatim ->
        "Unterminated verbatim.\n\
         Hint: Close with \"|}\"."

    type message = string Region.reg

    exception Error of message

    let fail region error =
      let msg = error_to_string error in
      raise (Error Region.{value=msg;region})

    (* TOKENS *)

    (* Making tokens *)

    let mk_string thread =
      let start    = thread#opening#start in
      let stop     = thread#closing#stop in
      let region   = Region.make ~start ~stop in
      Token.mk_string (thread#to_string) region

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in token, state

    let mk_bytes bytes state buffer =
      let State.{region; state; _} = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in token, state

    let mk_int state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token ->
          token, state
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      match Token.mk_nat lexeme region with
        Ok token ->
          token, state
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural
      | Error Token.Unsupported_nat_syntax ->
          fail region Unsupported_nat_syntax

    let mk_mutez state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      match Token.mk_mutez lexeme region with
        Ok token ->
          token, state
      | Error Token.Non_canonical_zero_tez ->
          fail region Non_canonical_zero
      | Error Token.Unsupported_mutez_syntax ->
          fail region Unsupported_mutez_syntax

    let mk_tez state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      let lexeme = Z.mul (Z.of_int 1_000_000) (Z.of_string lexeme) in
      match Token.mk_mutez (Z.to_string lexeme ^ "mutez") region with
        Ok token ->
          token, state
      | Error Token.Non_canonical_zero_tez ->
          fail region Non_canonical_zero
      | Error Token.Unsupported_mutez_syntax ->
          fail region Unsupported_mutez_syntax

    let format_tez s =
      match String.index s '.' with
        index ->
          let len         = String.length s in
          let integral    = Str.first_chars s index
          and fractional  = Str.last_chars s (len-index-1) in
          let num         = Z.of_string (integral ^ fractional)
          and den         = Z.of_string ("1" ^ String.make (len-index-1) '0')
          and million     = Q.of_string "1000000" in
          let mutez       = Q.make num den |> Q.mul million in
          let should_be_1 = Q.den mutez in
          if Z.equal Z.one should_be_1 then Some (Q.num mutez) else None
      | exception Not_found -> assert false

    let mk_tez_dec state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      let lexeme = Str.(global_replace (regexp "_") "" lexeme) in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      match format_tez lexeme with
        None -> assert false
      | Some tz ->
          match Token.mk_mutez (Z.to_string tz ^ "mutez") region with
            Ok token ->
              token, state
          | Error Token.Non_canonical_zero_tez ->
              fail region Non_canonical_zero
          | Error Token.Unsupported_mutez_syntax ->
              fail region Unsupported_mutez_syntax

    let mk_ident state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      let token = Token.mk_ident lexeme region
      in token, state

    let mk_attr attr state buffer =
      let State.{region; state; _} = state#sync buffer in
      let token = Token.mk_attr attr region
      in token, state

    let mk_uident state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      let token = Token.mk_uident lexeme region
      in token, state

    let mk_lang lang state buffer =
      let State.{region; state; _} = state#sync buffer in
      let start              = region#start#shift_bytes 1 in
      let stop               = region#stop in
      let lang_reg           = Region.make ~start ~stop in
      let lang               = Region.{value=lang; region=lang_reg} in
      match Token.mk_lang lang region with
        Ok token ->
          token, state
      | Error Token.Unsupported_lang_syntax ->
          fail region Unsupported_lang_syntax

    let mk_sym state buffer =
      let State.{region; lexeme; state} = state#sync buffer in
      match Token.mk_sym lexeme region with
        Ok token ->
          token, state
      | Error Token.Invalid_symbol s ->
          fail region (Invalid_symbol  s)

    let mk_eof state buffer =
      let State.{region; state; _} = state#sync buffer in
      let token = Token.mk_eof region
      in token, state

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let decimal    = natural '.' natural
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let letter     = small | capital
let ident      = small (letter | '_' | digit)* |
                 '_' (letter | '_' (letter | digit) | digit)+
let uident     = capital (letter | '_' | digit)*
let attr       = letter (letter | '_' | ':' | digit)*
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as b)
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)
let directive  = '#' (blank* as space) (small+ as id) (* For #include *)

(* Symbols *)

let     common_sym =   ";" | "," | "(" | ")"  | "[" | "]"  | "{" | "}"
                     | "=" | ":" | "|" | "." | "_" | "^"
                     | "+" | "-" | "*" | "/"  | "<" | "<=" | ">" | ">="
let  pascaligo_sym = "->" | "=/=" | "#" | ":="
let   cameligo_sym = "->" | "<>" | "::" | "||" | "&&" | "'"
let reasonligo_sym = "!" | "=>" | "!=" | "==" | "++" | "..." | "||" | "&&"
let     jsligo_sym = "++" | "--" | "..." | "?" | "&" | "!" | "~" | "%"
                     | "<<<" | "==" | "!=" | "+=" | "-=" | "*="
                     | "%=" | "<<<=" | "&=" | "|="
                     | "^=" | "=>" (* | ">>>" | ">>>=" *)

let symbol =
      common_sym
|  pascaligo_sym
|   cameligo_sym
| reasonligo_sym
|     jsligo_sym

(* RULES *)

(* The scanner [scan] has a parameter [state] that is threaded
   through recursive calls. *)

rule scan state = parse
  ident                  { mk_ident   state lexbuf }
| uident                 { mk_uident  state lexbuf }
| bytes                  { mk_bytes b state lexbuf }
| natural "n"            { mk_nat     state lexbuf }
| natural "mutez"        { mk_mutez   state lexbuf }
| natural ("tz" | "tez") { mk_tez     state lexbuf }
| decimal ("tz" | "tez") { mk_tez_dec state lexbuf }
| natural                { mk_int     state lexbuf }
| symbol                 { mk_sym     state lexbuf }
| eof                    { mk_eof     state lexbuf }
| "[@" (attr as a) "]"   { mk_attr  a state lexbuf }
| "[%" (attr as l)       { mk_lang  l state lexbuf }

| "`" | "{|" as lexeme {
    let verb_open, verb_close = Token.verbatim_delimiters in
    if lexeme = verb_open then
      let State.{region; state; _} = state#sync lexbuf in
      let thread = Thread.make ~opening:region
      in scan_verbatim verb_close thread state lexbuf
         |> mk_verbatim
    else
      let State.{region; _} = state#sync lexbuf
      in fail region (Unexpected_character lexeme.[0]) }

| _ as c { let State.{region; _} = state#sync lexbuf
           in fail region (Unexpected_character c) }

(* Scanning verbatim strings with or without inclusion of Michelson code *)
(* TODO: Move to preprocessor *)

and scan_verbatim verb_close thread state = parse
  '#' blank* (natural as line) blank+ '"' (string as file) '"'
  (blank+ ('1' | '2'))? blank* (nl | eof) {
      let State.{state; region; _} = state#sync lexbuf in
      let flag      = None in
      let linenum   = int_of_string line in
      let value     = linenum, file, flag in
      let directive = Directive.Linemarker Region.{value; region} in
      let state     = state#push_directive directive in
      let pos       = region#start#add_nl in
      let pos       = (pos#set_file file)#set_line linenum in
      let state     = state#set_pos pos
      in scan_verbatim verb_close thread state lexbuf }

| "`" | "|}" as lexeme {
        if verb_close = lexeme then
          State.(thread, (state#sync lexbuf).state)
        else
          let State.{state; _} = state#sync lexbuf
          and thread = thread#push_string lexeme in
          scan_verbatim verb_close thread state lexbuf }

| nl  { let nl     = Lexing.lexeme lexbuf in
        let ()     = Lexing.new_line lexbuf
        and state  = state#set_pos (state#pos#new_line nl)
        and thread = thread#push_string nl in
        scan_verbatim verb_close thread state lexbuf }

| eof { fail thread#opening Unterminated_verbatim }

| _   { let lexeme = Lexing.lexeme lexbuf in
        let State.{state; _} = state#sync lexbuf
        and thread = thread#push_string lexeme in
        scan_verbatim verb_close thread state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    let callback state lexbuf =
      try Stdlib.Ok (scan state lexbuf) with
        Error msg -> Stdlib.Error msg

  end (* of functor [Make] in HEADER *)
(* END TRAILER *)
}
