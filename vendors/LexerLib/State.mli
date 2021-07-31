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

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

type 'token t = <
  config        : 'token config;
  window        : 'token window option;
  pos           : Pos.t;
  set_pos       : Pos.t -> 'token t;
  slide_window  : 'token -> 'token t;
  sync          : Lexing.lexbuf -> 'token sync;
  decoder       : Uutf.decoder;
  supply        : Bytes.t -> int -> int -> unit;
  mk_line       :      Thread.t -> Markup.t * 'token t;
  mk_block      :      Thread.t -> Markup.t * 'token t;
  mk_newline    : Lexing.lexbuf -> Markup.t * 'token t;
  mk_space      : Lexing.lexbuf -> Markup.t * 'token t;
  mk_tabs       : Lexing.lexbuf -> Markup.t * 'token t;
  mk_bom        : Lexing.lexbuf -> Markup.t * 'token t;
  mk_linemarker : line:string ->
                  file:string ->
                  ?flag:char ->
                  Lexing.lexbuf ->
                  Directive.t * 'token t
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token t
}

type 'token state = 'token t

val make:
  config:  'token config ->
  window:  'token window option ->
  pos:     Pos.t ->
  decoder: Uutf.decoder ->
  supply:  (Bytes.t -> int -> int -> unit) ->
  'token t
