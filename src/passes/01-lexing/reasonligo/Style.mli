(* Checking style based on the lexical context *)

(* Vendor depedencies *)

module Region = Simple_utils.Region
module Unit   = LexerLib.Unit


(* Style checking function (filter-out) *)

type lex_units = Token.t Unit.t list

type message = string Region.reg

val check : (lex_units, message) result -> (lex_units, message) result
