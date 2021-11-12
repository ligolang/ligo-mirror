(* Self-passes on the lexical units for PascaLIGO *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Unit   = LexerLib.Unit

(* LIGO dependencies *)

module type TOKEN = Lexing_shared.Token.S

(* Functor *)

module Make (Token : TOKEN) =
  struct
    type lex_unit = Token.t Unit.t

    type units = lex_unit list

    type message = string Region.reg

    module Style = Style.Make (Token)

    let passes = [Style.filter]
  end
