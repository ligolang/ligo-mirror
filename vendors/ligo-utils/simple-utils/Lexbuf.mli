(* Side-effects on lexing buffers (see [Stdlib.Lexing]) *)

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
