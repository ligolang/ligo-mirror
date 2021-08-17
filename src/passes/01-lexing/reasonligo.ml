(* Interfacing the ReasonLIGO lexer. *)

(* LIGO dependencies *)

module Config = Preprocessing_reasonligo.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_reasonligo.Token)
