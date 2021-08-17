(* Interfacing the JsLIGO lexer. *)

(* LIGO dependencies *)

module Config = Preprocessing_jsligo.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_jsligo.Token)
