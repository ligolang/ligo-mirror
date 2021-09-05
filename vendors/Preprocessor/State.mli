(* State threaded along the scanning functions of API *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif, and #else *)

type cond  = If of mode | Elif of mode | Else
type trace = cond list

(* The type [state] groups the information that needs to be
   threaded along the scanning functions:

     * the field [config] records the source configuration;

     * the field [env] records the symbols defined by #define and not
       undefined by #undef;

     * the field [mode] informs whether the preprocessor is in
       copying or skipping mode;

     * the field [trace] is a stack of previous, still active
       conditional directives (this is support the parsing of
       conditionals without resorting to a parser generator like
       [menhir]);

     * the field [out] keeps the output buffer;

     * the field [chans] is a list of opened input channels (this is
       to keep track of embedded file inclusions by #include and close
       them when we are done);

     * the field [incl] is isomorphic to the file system path to the
       current input file, and it is changed to that of any included
       file;

     * the field [imports] is a list of (filename, module) imports
       (#import) *)

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

  (* DIRECTORIES *)

  push_dir : string -> state;
  path     : string;

  (* CONDITIONAL DIRECTIVES *)

  (* Predicate *)

  is_copy : bool;

  (* The method [reduce_cond] is called when a #endif directive is
     found, and the trace (see type [trace] above) needs updating. *)

  reduce_cond : (state, Error.t) Stdlib.result;

  (* The method [extend] is called when encountering conditional
     directives #if, #else and #elif. As its name suggests, it extends
     the current trace with the current conditional directive, whilst
     performing some validity checks. *)

  extend : cond -> mode -> (state, Error.t) Stdlib.result;

  (* Setting the trace *)

  set_trace : trace -> state;

  (* MODE *)

  (* Setting the mode *)

  set_mode : mode -> state;

  (* The function [last_mode] seeks the last mode as recorded in the
     trace (see type [trace] above). *)

  last_mode : mode;

  (* PRINTING *)

  (* Copying the current lexeme to the buffer if the mode is [Copy],
     otherwise a no-operation. *)

  copy : Lexing.lexbuf -> unit;

  (* End of lines are always copied. ALWAYS AND ONLY USE AFTER
     SCANNING newline characters (nl). *)

  proc_nl : Lexing.lexbuf -> unit;

  (* Copying a string *)

  print : string -> unit;

  (* SYMBOL ENVIRONMENT *)

  set_env       : Env.t -> state;
  add_symbol    : string -> state;
  remove_symbol : string -> state;

  (* INPUT CHANNELS *)

  set_chans : in_channel list -> state;
  push_chan : in_channel -> state;

  (* MODULE IMPORTS *)

  set_imports : (file_path * module_name) list -> state;
  push_import : file_path -> string -> state
>

and sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : state
}

type t = state

val empty : file:file_path -> state
