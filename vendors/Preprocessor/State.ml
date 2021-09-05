(* State threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif, and #else. *)

type cond  = If of mode | Elif of mode | Else
type trace = cond list

(* The type [state] groups the information that needs to be
   threaded along the scanning functions *)

type file_path = string
type module_name = string
type lexeme = string

type state = <
  env     : Env.t;
  mode    : mode;
  trace   : trace;
  out     : Buffer.t;
  chans   : in_channel list;
  incl    : file_path list;
  imports : (file_path * module_name) list;
  decoder : Uutf.decoder;
  supply  : Bytes.t -> int -> int -> unit;
  pos     : Pos.t;
  set_pos : Pos.t -> state;
  sync    : Lexing.lexbuf -> sync;

  newline    : Lexing.lexbuf -> state;
  mk_line    :      Thread.t -> Markup.t;
  mk_block   :      Thread.t -> Markup.t;
  mk_newline : Lexing.lexbuf -> Markup.t * state;
  mk_space   : Lexing.lexbuf -> Markup.t * state;
  mk_tabs    : Lexing.lexbuf -> Markup.t * state;
  mk_bom     : Lexing.lexbuf -> Markup.t * state;

  (* Directories *)

  push_dir : string -> state;
  path     : string;

  (* CONDITIONAL DIRECTIVES *)

  is_copy     : bool;
  reduce_cond : (state, Error.t) Stdlib.result;
  extend      : cond -> mode -> (state, Error.t) Stdlib.result;
  set_trace   : trace -> state;

  (* Mode *)

  set_mode  : mode -> state;
  last_mode : mode;

  (* Printing *)

  copy    : Lexing.lexbuf -> unit;
  proc_nl : Lexing.lexbuf -> unit;
  print   : string        -> unit;

  (* Symbol environment *)

  set_env       : Env.t -> state;
  add_symbol    : string -> state;
  remove_symbol : string -> state;

  (* Input channels *)

  set_chans : in_channel list -> state;
  push_chan : in_channel -> state;

  (* Imports *)

  set_imports : (file_path * module_name) list -> state;
  push_import : file_path -> string -> state
>

and sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : state
}

type t = state

let empty ~file =
  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  object (self)
    val env = Env.empty
    method env = env

    val mode = Copy
    method mode = mode

    val trace = []
    method trace = trace

    val out = Buffer.create 80
    method out = out

    val chans = []
    method chans = chans

    val incl = [Filename.dirname file]
    method incl = incl

    val imports = []
    method imports = imports

    method decoder = decoder
    method supply  = Uutf.Manual.src decoder

    val pos    = Pos.min ~file
    method pos = pos

    method set_pos pos = {< pos = pos >}

    method sync lexbuf : sync =
      let lexeme = Lexing.lexeme lexbuf in
      let length = String.length lexeme
      and start  = pos in
      let stop   = start#shift_bytes length in
      let state  = {< pos = stop >}
      and region = Region.make ~start:pos ~stop
      in {region; lexeme; state}

    (* MARKUP *)

    method newline lexbuf =
      let () = Lexing.new_line lexbuf in
      let nl = Lexing.lexeme lexbuf in
      self#set_pos (self#pos#new_line nl)

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
      Markup.LineCom Region.{region; value}

    method mk_block thread =
      let start  = thread#opening#start in
      let region = Region.make ~start ~stop:self#pos
      and value  = thread#to_string in
      Markup.BlockCom Region.{region; value}

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

    (* Directories *)

    method push_dir dir =
      if dir = "." then self else {< incl = dir::incl >}

    method path =
      String.concat Filename.dir_sep (List.rev incl)

    (* CONDITIONAL DIRECTIVES *)

    method is_copy = (mode = Copy)

    method reduce_cond =
      let rec reduce = function
                    [] -> Stdlib.Error Error.Dangling_endif
      | If mode::trace -> Stdlib.Ok {< mode; trace >}
      |       _::trace -> reduce trace
      in reduce trace

    method extend cond mode =
      match cond, trace with
        If _,   Elif _::_ -> Stdlib.Error Error.If_follows_elif
      | Else,     Else::_ -> Stdlib.Error Error.Else_follows_else
      | Else,          [] -> Stdlib.Error Error.Dangling_else
      | Elif _,   Else::_ -> Stdlib.Error Error.Elif_follows_else
      | Elif _,        [] -> Stdlib.Error Error.Dangling_elif
      | hd,            tl -> Stdlib.Ok {< trace = hd::tl; mode >}

    method set_trace trace = {< trace >}

    (* MODE *)

    method set_mode mode = {< mode >}

    method last_mode =
      let rec aux = function
                              [] -> Copy (* Should not happen *)
      | (If mode | Elif mode)::_ -> mode
      |                 _::trace -> aux trace
      in aux trace

    (* PRINTING *)

    method copy buffer =
      if self#is_copy then Buffer.add_string out (Lexing.lexeme buffer)

    method proc_nl buffer = Lexing.new_line buffer; self#copy buffer

    method print string = Buffer.add_string out string

    (* SYMBOL ENVIRONMENT *)

    method set_env env      = {< env >}
    method add_symbol id    = {< env = Env.add id env >}
    method remove_symbol id = {< env = Env.remove id env >}

    (* INPUT CHANNELS *)

    method set_chans chans   = {< chans >}
    method push_chan in_chan = {< chans = in_chan :: chans >}

    (* MODULE IMPORTS *)

    method set_imports imports = {< imports >}

    method push_import path imported_module =
      {< imports = (path, imported_module) :: imports >}
  end
