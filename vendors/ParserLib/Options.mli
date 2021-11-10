(* CLI options of ParserLib only *)

module type S =
  sig
    (* We cumulate the lexer options *)

    include LexerLib.Options.S

    (* Use the monolithic API of Menhir. Default is incremental API. *)

    val mono : bool  (* --mono *)

    (* Pretty-print the input. *)

    val pretty : bool  (* --pretty *)

    (* Print the AST in ASCII-art. *)

    val cst : bool  (* --cst *)

    (* Reconstruct the tokens from the CST and print them .*)

    val cst_tokens : bool  (* --cst-tokens *)

    (* Enable error recovery *)

    val recovery : bool  (* --recovery *)

    (* Enable tracing of error recovery (debug option) *)

    val trace_recovery : bool  (* --trace-recovery *)

    (* File path where tracing will be printed ([None] means STDOUT) *)

    val trace_recovery_output : string option
  end
