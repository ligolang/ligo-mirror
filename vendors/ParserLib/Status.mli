(* Status after parsing the CLI *)

type t = LexerLib.Status.t  (* Lexer CLI options *)

type status = t

val status : t
