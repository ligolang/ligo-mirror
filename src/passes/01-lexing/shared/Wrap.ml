(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Wrapping tokens with metadata *)

type attr_key = string

type attr_val = AttrString of string

type attribute = attr_key * attr_val option

type attributes = attribute list

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

let payload wrap =
  Region.{region=wrap#region; value=wrap#payload}
