(* This module defines a type to support the lexing of strings and
   comments. Read README.md *)

module Region = Simple_utils.Region

type t = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> t;
  push_string : string -> t;
  set_opening : Region.t -> t
>

type thread = t

val make : Region.t -> t
