(* Data stored in strings destined to [stdout] and [stderr] *)

type t = {out : string; err : string}

type std = t

val empty   : t
val add_out : string -> t -> t
val add_err : string -> t -> t
val redden  : string -> string
