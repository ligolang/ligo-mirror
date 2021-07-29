(* Definition of the state threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* Configuration *)

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type command = [`Copy | `Units | `Tokens] option

type lexeme = string
type file_path = string

type 'token config = <
  block     : block_comment option;
  line      : line_comment option;
  input     : file_path option;
  offsets   : bool;
  mode      : [`Byte | `Point];
  command   : command;
  is_eof    : 'token -> bool;
  to_region : 'token -> Region.t;
  to_lexeme : 'token -> string;
  to_string : offsets:bool -> [`Byte | `Point] -> 'token -> string
>

(* State *)

type 'token lex_unit =
  Token     of 'token
| Markup    of Markup.t
| Directive of Directive.t

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

type 'token t = <
  config       : 'token config;
  window       : 'token window option;
  pos          : Pos.t;
  set_pos      : Pos.t -> 'token t;
  slide_window : 'token -> 'token t;
  sync         : Lexing.lexbuf -> 'token sync;
  decoder      : Uutf.decoder;
  supply       : Bytes.t -> int -> int -> unit;
  mk_line      :      Thread.t -> 'token lex_unit * 'token t;
  mk_block     :      Thread.t -> 'token lex_unit * 'token t;
  mk_newline   : Lexing.lexbuf -> 'token lex_unit * 'token t;
  mk_space     : Lexing.lexbuf -> 'token lex_unit * 'token t;
  mk_tabs      : Lexing.lexbuf -> 'token lex_unit * 'token t;
  mk_bom       : Lexing.lexbuf -> 'token lex_unit * 'token t
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token t
}

type 'token state = 'token t

val make:
  config: 'token config ->
  window: 'token window option ->
  pos: Pos.t ->
  decoder: Uutf.decoder ->
  supply:(Bytes.t -> int -> int -> unit) ->
  'token t

(* Updating the state after scanning a linemarker (a preprocessing
   directive generated by the preprocessor). *)

val linemarker :
  Region.t ->
  line:string ->
  file:string ->
  ?flag:char ->
  'token state ->
  Lexing.lexbuf ->
  'token lex_unit * 'token state
