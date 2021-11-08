(* Printing the tokens from the Concrete Syntax Tree (CST) *)

(* This module produces tokens reconstructed from the Concrete Syntax
   Tree (CST) in the same way they should be produced by the lexer. In
   other words, the leaves of the CST are printed as tokens. This
   enables to test the transmission of the tokens from the lexer to
   the parser. *)

(* STATE *)

(* The printing of the tokens makes use of a threaded data structure:
   the _state_. The arguments to the function [mk_state] are as
   follows:

     * The first argument is an optional string buffer. If present,
       the result of the printing functions below will be added to it.

     * The parameter labelled [offsets] is [true] if source locations
       make use of horizontal offsets (like Emacs), and column numbers
       (like Vim) otherwise.

     * The parameter of type [[`Point|`Byte]], depending whether we
       assume purely byte-oriented input, for example ASCII, or
       general unicode, like UTF-8. *)

type state

val mk_state :
  ?buffer:Buffer.t -> offsets:bool -> [`Point|`Byte] -> state

(* Printing tokens *)

type ('src, 'dst) printer = state -> 'src -> 'dst

(* IMPORTANT: If you add or remove a printing function, please mirror
   your changes in the aliases below accordingly. If you export
   functions printing special nodes of the CST, please call them
   "print_<node>_to_<dest>". *)

(* The functions here are perhaps best when opening this module first,
   e.g. [let open PrintTokens in print_to_buffer t]. *)

val print_to_buffer : (CST.t, Buffer.t) printer
val print_to_string : (CST.t,   string) printer

(* Aliases (preferably use fully qualified,
   e.g. [PrintTokens.to_buffer]) *)

val to_buffer : (CST.t, Buffer.t) printer
val to_string : (CST.t,   string) printer
