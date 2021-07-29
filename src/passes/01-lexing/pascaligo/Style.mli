(* Checking style based on the lexical context *)

(* Vendor depedencies *)

module Region = Simple_utils.Region
module State  = LexerLib.State

(* Style checking function (filter-out) *)

type lex_units = Token.t State.lex_unit list

type message = string Region.reg

val check : (lex_units, message) result -> (lex_units, message) result
