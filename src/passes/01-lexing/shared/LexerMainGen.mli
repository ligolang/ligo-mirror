(* This module is a wrapper for running the LIGO lexers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

module type CONFIG  = Preprocessor.Config.S
module type OPTIONS = LexerLib.Options.S
module type TOKEN   = Token.S

(* This module factors the common actions expected from LexerMain in
   all LIGO syntaxes, like reading and checking the command-line,
   building the preprocessor, the lexer, composing them and calling
   them. Note the use of a generative functor to remind the callers
   that a side-effect is performed (reading from and writing to
   [Sys.argv]: see module [LexerLib.CLI].). *)

module Make (Config      : CONFIG)
            (Options     : OPTIONS)
            (Token       : TOKEN)
            (Self_tokens : Self_tokens.S with type token = Token.t) :
  sig
    module Token : TOKEN
    type token = Token.t

    (* Scanning one token *)

    type window = <
      last_token    : token option;
      current_token : token (* Including EOF *)
    >

    type message = string Region.reg

    val scan : Lexing.lexbuf -> (token, message) Stdlib.result

    val get_window : unit -> window option

    val clear : unit -> unit

    (* Scanning all tokens in the input given by the CLI, after the
       preprocessor is run. *)

    val scan_all : unit -> unit

    (* Check the CLI *)

    val check_cli : unit -> unit
  end with module Token = Token
