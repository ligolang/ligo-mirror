(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Attributes *)

type attr_key = string

type attr_val = AttrString of string

type attribute = attr_key * attr_val option

type attributes = attribute Region.reg list

let attr_to_lexeme (key, value_opt) =
  match value_opt with
    None -> Printf.sprintf "[@%s]" key
  | Some AttrString value -> Printf.sprintf "[@%s %s]" key value

let attr_to_string (key, value_opt) =
  match value_opt with
    None -> Printf.sprintf "%S" key
  | Some AttrString value -> Printf.sprintf "(%S, %s)" key value

(* Wrapping tokens with metadata *)

type 'payload wrap = <
  payload    : 'payload;
  attributes : attributes;
  region     : Region.t;

  set_attributes : attributes -> 'payload wrap
>

type 'a t = 'a wrap

let wrap ?(attributes=[]) payload region =
  object
    method payload    = payload
    val attributes    = attributes
    method attributes = attributes
    method region     = region

    method set_attributes attr = {< attributes = attr >}
  end

let wrap_ghost payload = wrap payload Region.ghost
let ghost = wrap_ghost

let payload wrap =
  Region.{region=wrap#region; value=wrap#payload}
