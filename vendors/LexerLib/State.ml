(* Definition of the state threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

type lexeme = string

(*
type 'token window = <
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

let empty ~file =
  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  object (self)
    val pos        = Pos.min ~file
    method pos     = pos
    method decoder = decoder
    method supply  = Uutf.Manual.src decoder

    method set_pos pos = {< pos = pos >}

    val lexical_units = []
    method lexical_units = lexical_units

    method push_unit unit =
      {< lexical_units = unit :: self#lexical_units >}

    method newline lexbuf =
      let () = Lexing.new_line lexbuf in
      let nl = Lexing.lexeme lexbuf in
      self#set_pos (self#pos#new_line nl)

    method push_token token =
      {< lexical_units = `Token token :: self#lexical_units >}

    (* The call [state#slide_window token] pushes the token [token] in
       the buffer [lexbuf]. If the buffer is full, that is, it is [Two
       (t1,t2)], then the token [t2] is discarded to make room for
       [token]. *)
(*
    method slide_window new_token =
      let new_window =
        match self#window with
          None ->
            object
              method last_token    = None
              method current_token = new_token
            end
        | Some window ->
            object
              method last_token    = Some window#current_token
              method current_token = new_token
            end
      in {< window = Some new_window >}
 *)

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
      let markup = Markup.Newline Region.{region; value} in
      let state  = self#set_pos pos
      in state#push_unit (`Markup markup)

    method push_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.LineCom Region.{region; value}
      in self#push_unit (`Markup markup)

    method push_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.BlockCom Region.{region; value}
      in self#push_unit (`Markup markup)

    method push_space lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Space Region.{region; value}
      in state#push_unit (`Markup markup)

    method push_tabs lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Tabs Region.{region; value}
      in state#push_unit (`Markup markup)

    method push_bom lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = lexeme in
      let markup = Markup.BOM Region.{region; value}
      in state#push_unit (`Markup markup)

    method push_linemarker ~line ~file ?flag lexbuf =
      let {state; region; _} = self#sync lexbuf in
      let flag      = match flag with
                        Some '1' -> Some Directive.Push
                      | Some '2' -> Some Directive.Pop
                      | _        -> None in
      let linenum   = int_of_string line in
      let value     = linenum, file, flag in
      let directive = Directive.Linemarker Region.{value; region} in
      let pos       = region#start#add_nl in
      let pos       = (pos#set_file file)#set_line linenum in
      let state     = state#set_pos pos
      in state#push_unit (`Directive directive)
  end
