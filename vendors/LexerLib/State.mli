(* Definition of the state threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* State *)

type lexeme = string

(* type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
> *)

type 'token state = <
  pos           : Pos.t;
  set_pos       : Pos.t -> 'token state;
  sync          : Lexing.lexbuf -> 'token sync;
  decoder       : Uutf.decoder;
  supply        : Bytes.t -> int -> int -> unit;
  newline       : Lexing.lexbuf -> 'token state;

  push_token    : 'token -> 'token state;

  lexical_units : 'token Unit.t list;
  push_unit     : 'token Unit.t -> 'token state;

  push_line       :      Thread.t -> 'token state;
  push_block      :      Thread.t -> 'token state;
  push_newline    : Lexing.lexbuf -> 'token state;
  push_space      : Lexing.lexbuf -> 'token state;
  push_tabs       : Lexing.lexbuf -> 'token state;
  push_bom        : Lexing.lexbuf -> 'token state;

  push_linemarker : line:string   ->
                    file:string   ->
                    ?flag:char    ->
                    Lexing.lexbuf -> 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}

type 'token t = 'token state

val empty : file:string -> 'token state
