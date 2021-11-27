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

val wrap : ?attributes:Attr.attributes -> 'a -> Region.t -> 'a wrap

val ghost : 'a -> 'a wrap
