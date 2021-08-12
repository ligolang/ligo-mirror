(* Status after parsing the CLI *)

type t = [
  `Done
| `Version      of string
| `Help         of Buffer.t
| `CLI          of Buffer.t
| `SyntaxError  of string
| `FileNotFound of string
| `WrongFileExt of string
]

type status = t

val status : t
