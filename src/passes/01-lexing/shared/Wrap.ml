(* Wrapping attributes and regions with tokens *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Wrapping tokens with metadata *)

type 'payload wrap = <
  payload    : 'payload;
  attributes : Attr.attributes;
  region     : Region.t;

  set_attributes : Attr.attributes -> 'payload wrap
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
