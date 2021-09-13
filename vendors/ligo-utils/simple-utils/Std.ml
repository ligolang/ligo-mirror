type t = {out : string; err : string}

type std = t

let empty = {out = ""; err = ""}

let add_out out std =
  if std.out = "" then {std with out}
  else {std with out = std.out ^ "\n" ^ out}

let add_err err std =
  if std.err = "" then {std with err}
  else {std with err = std.err ^ "\n" ^ err}

let redden string = Printf.sprintf "\027[31m%s\027[0m" string
