(* Interfacing the Michelson lexer. *)

(* LIGO dependencies *)

module Config = Preprocessing_michelson.Config

(* Internal dependencies *)

include Lexing_shared.Common.Make (Config) (Lexing_michelson.Token)
