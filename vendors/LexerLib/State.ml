(* Definition of the state threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

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

let make ~config ~window ~pos ~decoder ~supply : 'token state =
  object (self)
    method config  = config
    val window     = window
    method window  = window
    val pos        = pos
    method pos     = pos
    method decoder = decoder
    method supply  = supply

    method set_pos pos = {< pos = pos >}

    (* The call [state#slide_window token] pushes the token [token] in
       the buffer [lexbuf]. If the buffer is full, that is, it is [Two
       (t1,t2)], then the token [t2] is discarded to make room for
       [token]. *)

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

    method mk_newline lexbuf =
      let ()     = Lexing.new_line lexbuf in
      let value  = Lexing.lexeme lexbuf in
      let start  = self#pos in
      let stop   = start#new_line value in
      let region = Region.make ~start ~stop in
      let markup = Markup.Newline Region.{region; value}
      in markup, self#set_pos stop

    method mk_line thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.LineCom Region.{region; value}
      in markup, self

    method mk_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      let markup = Markup.BlockCom Region.{region; value}
      in markup, self

    method mk_space lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Space Region.{region; value}
      in markup, state

    method mk_tabs lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = String.length lexeme in
      let markup = Markup.Tabs Region.{region; value}
      in markup, state

    method mk_bom lexbuf =
      let {region; lexeme; state} = self#sync lexbuf in
      let value  = lexeme in
      let markup = Markup.BOM Region.{region; value}
      in markup, state

    method mk_linemarker ~line ~file ?flag lexbuf =
      let {state; region; _} = self#sync lexbuf in
      let flag      = match flag with
                        Some '1' -> Some Directive.Push
                      | Some '2' -> Some Directive.Pop
                      | _        -> None in
      let linenum   = int_of_string line in
      let value     = linenum, file, flag in
      let directive = Directive.Linemarker Region.{value; region} in
      let pos       = region#start#add_nl in
      let pos       = (pos#set_file file)#set_line linenum
      in directive, state#set_pos pos
  end
