(* TODO: write description *)

type t

val make : string option -> t option

val get_root_inclusion_list : string option -> string list

val find_external_file : string -> string list -> (string * in_channel) option

val get_includes : string -> t option -> string list