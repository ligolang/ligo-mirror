(* Attributes *)

module Region = Simple_utils.Region

type key = string

type value = String of string

type attribute = key * value option

type t = attribute

type attributes = attribute Region.reg list

let to_lexeme (key, value_opt) =
  match value_opt with
    None -> Printf.sprintf "[@%s]" key
  | Some String value -> Printf.sprintf "[@%s %s]" key value

let to_string (key, value_opt) =
  match value_opt with
    None -> Printf.sprintf "%S" key
  | Some String value -> Printf.sprintf "(%S, %s)" key value
