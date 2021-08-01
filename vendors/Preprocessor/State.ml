(* Definition of the state threaded along the scanning functions of API *)

(* Internal dependencies *)

open Error

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif, #else,
   #region and #endregion. *)

type cond  = If of mode | Elif of mode | Else | Region
type trace = cond list

(* The type [state] groups the information that needs to be
   threaded along the scanning functions *)

type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type file_path = string
type module_name = string

type config = <
  block   : block_comment option;
  line    : line_comment option;
  input   : file_path option;
  offsets : bool;
  dirs    : file_path list (* Directories to search for #include files *)
>

type state = {
  config : config;
  env    : E_AST.Env.t;
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
  |      Region::_ -> Stdlib.Error Open_region_in_conditional
  |       _::trace -> reduce trace
  in reduce state.trace

(* The function [reduce_region] is called when a #endregion directive
   is read, and the trace needs updating. *)

let reduce_region state =
  match state.trace with
               [] -> Stdlib.Error Dangling_endregion
  | Region::trace -> Stdlib.Ok {state with trace}
  |             _ -> Stdlib.Error Conditional_in_region

(* The function [extend] is called when encountering conditional
   directives #if, #else and #elif. As its name suggests, it extends
   the current trace with the current conditional directive, whilst
   performing some validity checks. *)

let extend cond mode state =
  match cond, state.trace with
    If _, Elif _::_ -> Stdlib.Error If_follows_elif
  | Else,   Else::_ -> Stdlib.Error Else_follows_else
  | Else,        [] -> Stdlib.Error Dangling_else
  | Elif _, Else::_ -> Stdlib.Error Elif_follows_else
  | Elif _,      [] -> Stdlib.Error Dangling_elif
  |     hd,     tl  -> Stdlib.Ok {state with trace = hd::tl; mode}

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

(* End of lines are always copied. ALWAYS AND ONLY USE AFTER SCANNING nl. *)

let proc_nl state buffer =
  Lexing.new_line buffer; copy state buffer

(* Copying a string *)

let print state string = Buffer.add_string state.out string

(* SYMBOL ENVIRONMENT *)

let env_add id state = {state with env = E_AST.Env.add id state.env}

let env_rem id state = {state with env = E_AST.Env.remove id state.env}

(* INPUT CHANNELS *)

let push_chan in_chan state = {state with chans = in_chan :: state.chans}

(* FINDING FILES *)

type dir = string

let open_file path state =
  let in_chan = open_in path in
  let state   = push_chan in_chan state
  in path, in_chan, state

let find file_path state =
  let rec aux = function
           [] -> Stdlib.Error (Error.File_not_found file_path)
  | dir::dirs -> let path =
                if dir = "." || dir = "" then file_path
                else dir ^ Filename.dir_sep ^ file_path in
                try Stdlib.Ok (open_file path state) with
                  Sys_error _ -> aux dirs
  in aux state.config#dirs

let find dir file state =
  let path =
    if dir = "." || dir = "" then file
    else dir ^ Filename.dir_sep ^ file in
  try
    Stdlib.Ok (open_file path state) with
    Sys_error _ ->
      let base = Filename.basename file in
      if base = file then find file state
      else Stdlib.Error (Error.File_not_found file)

(* IMPORTS *)

let push_import path imported_module state =
  {state with import = (path, imported_module) :: state.import}
