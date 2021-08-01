(* This module defines a type to support the lexing of strings and
   comments. Read README.md *)

module Region = Simple_utils.Region

type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>

type t = thread

val make : Region.t -> t
