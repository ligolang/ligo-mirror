(* Definition of the state threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* State *)

type lexeme = string

type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>

type 'token state = <
  window        : 'token window option;
  pos           : Pos.t;
  set_pos       : Pos.t -> 'token state;
  slide_window  : 'token -> 'token state;
  sync          : Lexing.lexbuf -> 'token sync;
  decoder       : Uutf.decoder;
  supply        : Bytes.t -> int -> int -> unit;
  newline       : Lexing.lexbuf -> 'token state;
  mk_line       :      Thread.t -> Markup.t;
  mk_block      :      Thread.t -> Markup.t;
  mk_newline    : Lexing.lexbuf -> Markup.t * 'token state;
  mk_space      : Lexing.lexbuf -> Markup.t * 'token state;
  mk_tabs       : Lexing.lexbuf -> Markup.t * 'token state;
  mk_bom        : Lexing.lexbuf -> Markup.t * 'token state;
  mk_linemarker : line:string ->
                  file:string ->
                  ?flag:char ->
                  Lexing.lexbuf ->
                  Directive.t * 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}

type 'token t = 'token state

val empty : file:string -> 'token state
