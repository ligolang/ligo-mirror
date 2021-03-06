(* Preprocessing errors for the compiler *)

let stage = "preprocessing"

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Location = Simple_utils.Location
module Snippet  = Simple_utils.Snippet
module Display  = Simple_utils.Display

(* Errors *)

type t = [
  `Preprocessing_generic of string Region.reg
]

type error = t

let generic reg = `Preprocessing_generic reg

(* Colour snippet *)

let error_ppformat :
      display_format:(string Display.display_format) ->
      Format.formatter ->
      error ->
      unit =
  fun ~display_format format error ->
  match display_format with
    Human_readable | Dev ->
      match error with
        `Preprocessing_generic Region.{region; value} ->
           Snippet.pp_lift format region;
           Format.pp_print_string format value

(* JSON *)

let error_jsonformat : error -> Yojson.Safe.t =
  fun error ->
    let json_error ~stage ~content =
      `Assoc [
         ("status", `String "error");
         ("stage",  `String stage);
         ("content", content)] in
    match error with
      `Preprocessing_generic Region.{region; value} ->
         let loc = Location.to_yojson @@ Location.lift region in
         let content =
           `Assoc [
              ("message",  `String value);
              ("location",  loc)]
         in json_error ~stage ~content
