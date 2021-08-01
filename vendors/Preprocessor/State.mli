(* Definition of the state threaded along the scanning functions of API *)

(* The type [mode] defines the two scanning modes of the preprocessor:
   either we copy the current characters or we skip them. *)

type mode = Copy | Skip

(* Trace of directives. We keep track of directives #if, #elif, #else,
   #region and #endregion. *)

type cond  = If of mode | Elif of mode | Else | Region
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

     * the field [import] is a list of (filename, module) imports
       (#import) *)

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

(* MODE *)

val last_mode : trace -> mode

(* DIRECTORIES *)

val push_dir : string -> state -> state
val mk_path  : state -> string

(* STATE REDUCTIONS/EXTENSIONS *)

(* The function [reduce_cond] is called when a #endif directive is
   found, and the trace (see type [trace] above) needs updating.

   The function [reduce_region] is called when a #endregion directive
   is read, and the trace needs updating.

   The function [extend] is called when encountering conditional
   directives #if, #else and #elif. As its name suggests, it extends
   the current trace with the current conditional directive, whilst
   performing some validity checks. *)

val reduce_cond   : t -> (t, Error.t) Stdlib.result
val reduce_region : t -> (t, Error.t) Stdlib.result
val extend        : cond -> mode -> t -> (t, Error.t) Stdlib.result

(* PRINTING *)

val copy    : t -> Lexing.lexbuf -> unit
val proc_nl : t -> Lexing.lexbuf -> unit
val print   : t -> string        -> unit

(* SYMBOL ENVIRONMENT *)

val env_add : string -> state -> state
val env_rem : string -> state -> state

(* FINDING FILES *)

type dir = string

val find :
  dir ->
  file_path ->
  state ->
  (file_path * in_channel * t, Error.t) Stdlib.result

(* INPUT CHANNELS *)

val push_chan : in_channel -> state -> state

(* IMPORTS *)

val push_import : file_path -> string -> state -> state
