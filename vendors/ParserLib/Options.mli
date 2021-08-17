(* Gathering CLI options *)

module type S =
  sig
    (* We cumulate the lexer options *)

    include LexerLib.Options.S

    (* CLI options of ParserLib only *)

    val mono       : bool  (* --mono       *)
    val pretty     : bool  (* --pretty     *)
    val cst        : bool  (* --cst        *)
    val cst_tokens : bool  (* --cst-tokens *)
end
