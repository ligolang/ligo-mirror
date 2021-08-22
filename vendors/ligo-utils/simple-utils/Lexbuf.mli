(* Side-effects on lexing buffers (see [Stdlib.Lexing]) *)

(* Line number of the current position in the lexing buffer *)

val current_linenum  : Lexing.lexbuf -> int

(* File name of the start position in the lexing buffer. *)

val current_filename : Lexing.lexbuf -> string

(* Rolling back one lexeme _within the current semantic action_ *)

val rollback : Lexing.lexbuf -> unit

(* Resetting file name and/or line number and/or offset in a lexing
   buffer. *)

type file_path = string

val reset :
  ?file:file_path -> ?line:int -> ?offset:int -> Lexing.lexbuf -> unit

val reset_file   : string -> Lexing.lexbuf -> unit
val reset_line   : int -> Lexing.lexbuf -> unit
val reset_offset : int -> Lexing.lexbuf -> unit
