(* Definition of the state threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

type lexeme = string

type 'token state = <
  pos            : Pos.t;
  set_pos        : Pos.t -> 'token state;
  sync           : Lexing.lexbuf -> 'token sync;
  decoder        : Uutf.decoder;
  supply         : Bytes.t -> int -> int -> unit;
  newline        : Lexing.lexbuf -> 'token state;
  lexical_units  : 'token Unit.lex_unit list;

  push_token     :        'token -> 'token state;
  push_directive :   Directive.t -> 'token state;
  push_markup    :      Markup.t -> 'token state;

  push_line      :      Thread.t -> 'token state;
  push_block     :      Thread.t -> 'token state;
  push_newline   : Lexing.lexbuf -> 'token state;
  push_space     : Lexing.lexbuf -> 'token state;
  push_tabs      : Lexing.lexbuf -> 'token state;
  push_bom       : Lexing.lexbuf -> 'token state;
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}

type 'token t = 'token state

let empty ~file : 'token t =
  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  object (self)
    val pos        = Pos.min ~file
    method pos     = pos
    method decoder = decoder
    method supply  = Uutf.Manual.src decoder

    method set_pos pos = {< pos = pos >}

    val lexical_units = []
    method lexical_units = lexical_units

    method push_token token =
      {< lexical_units = (`Token token) :: self#lexical_units >}

    method push_directive dir =
      {< lexical_units = (`Directive dir) :: self#lexical_units >}

    method push_markup mark =
      {< lexical_units = (`Markup mark) :: self#lexical_units >}

    method newline lexbuf =
      let () = Lexing.new_line lexbuf in
      let nl = Lexing.lexeme lexbuf in
      self#set_pos (self#pos#new_line nl)

    method sync lexbuf : 'token sync =
      let lexeme = Lexing.lexeme lexbuf in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in {region; lexeme; state}

    (* MARKUP *)

    (* Committing markup to the current logical state *)

    method push_newline lexbuf =
      let ()     = Lexing.new_line lexbuf in
      let value  = Lexing.lexeme lexbuf in
      let start  = self#pos in
      let stop   = start#new_line value in
      let region = Region.make ~start ~stop in
      let markup = Markup.Newline Region.{region; value}
      in (self#push_markup markup)#set_pos pos

    method push_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.LineCom Region.{region; value}
      in self#push_markup markup

    method push_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.BlockCom Region.{region; value}
      in self#push_markup markup

    method push_space lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Space Region.{region; value}
      in state#push_markup markup

    method push_tabs lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Tabs Region.{region; value}
      in state#push_markup markup

    method push_bom lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = lexeme in
      let markup = Markup.BOM Region.{region; value}
      in state#push_markup markup
  end
