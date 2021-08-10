(* Definition of the state threaded along the scanning functions of API *)

(* Internal dependencies *)

open Error

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

type state = {
  env    : Env.t;
  mode   : mode;
  trace  : trace;
  out    : Buffer.t;
  chans  : in_channel list;
  incl   : file_path list;
  import : (file_path * module_name) list
}

type t = state

(* Directories *)

let push_dir dir state =
  if dir = "." then state else {state with incl = dir :: state.incl}

let mk_path state =
  String.concat Filename.dir_sep (List.rev state.incl)

(* The function [reduce_cond] is called when a #endif directive is
   found, and the trace (see type [trace] above) needs updating. *)

let reduce_cond state =
  let rec reduce = function
                [] -> Stdlib.Error Dangling_endif
  | If mode::trace -> Stdlib.Ok {state with mode; trace}
  |       _::trace -> reduce trace
  in reduce state.trace

(* The function [extend] is called when encountering conditional
   directives #if, #else and #elif. As its name suggests, it extends
   the current trace with the current conditional directive, whilst
   performing some validity checks. *)

let extend cond mode state =
  match cond, state.trace with
    If _,   Elif _::_ -> Stdlib.Error If_follows_elif
  | Else,     Else::_ -> Stdlib.Error Else_follows_else
  | Else,          [] -> Stdlib.Error Dangling_else
  | Elif _,   Else::_ -> Stdlib.Error Elif_follows_else
  | Elif _,        [] -> Stdlib.Error Dangling_elif
  | hd,            tl -> Stdlib.Ok {state with trace = hd::tl; mode}

(* The function [last_mode] seeks the last mode as recorded in the
   trace (see type [trace] above). *)

let rec last_mode = function
                        [] -> assert false  (* TODO: Remove assertion *)
| (If mode | Elif mode)::_ -> mode
|                 _::trace -> last_mode trace

(* PRINTING *)

(* Copying the current lexeme to the buffer *)

let copy state buffer =
  Buffer.add_string state.out (Lexing.lexeme buffer)

(* End of lines are always copied.
   ALWAYS AND ONLY USE AFTER SCANNING newline characters (nl). *)

let proc_nl state buffer =
  Lexing.new_line buffer; copy state buffer

(* Copying a string *)

let print state string = Buffer.add_string state.out string

(* SYMBOL ENVIRONMENT *)

let env_add id state = {state with env = Env.add id state.env}

let env_rem id state = {state with env = Env.remove id state.env}

(* INPUT CHANNELS *)

let push_chan in_chan state = {state with chans = in_chan :: state.chans}

(* IMPORTS *)

let push_import path imported_module state =
  {state with import = (path, imported_module) :: state.import}
