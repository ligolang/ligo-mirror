(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Config  = Preprocessor.Config
module Options = LexerLib.Options

(* This module factors the common actions expected from LexerMain in
   all LIGO syntaxes, like reading and checking the command-line,
   building the preprocessor, the lexer, composing them and calling
   them. Note the use of a generative functor to remind the callers
   that a side-effect is performed (reading from and writing to
   [Sys.argv]: see module [LexerLib.CLI].). *)

module Make (Config      : Config.S)
            (Options     : Options.S)
            (Token'      : Token.S)
            (Self_tokens : Self_tokens.S with type token = Token'.t) :
  sig
    module Token : Token.S
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
  end with module Token = Token'
