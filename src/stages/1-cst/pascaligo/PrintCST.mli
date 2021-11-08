(* Printing a digest of the Concrete Syntax Tree (CST) as ASCII *)

(* This module prints the CST  a nice branching layout with some of
   the most relevant nodes and leaves. This enables visually testing,
   for example, whether the parser properly enforces the expected
   associativity of a given operator. The printing of the CST can be
   requested on the command-line of the LIGO compiler with the option
   "print-cst" (see file [src/bin/cli.ml]). *)

(* STATE *)

(* The printing of the CST itself makes use of a threaded data
   structure: the _state_. The fields for padding the beggining of the
   lines in the tree layout, [pad_path] and [pad_node], make up the
   tree layout. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) *)

type state

val mk_state :
  ?buffer:Buffer.t -> offsets:bool -> [`Point|`Byte] -> state

(* IMPORTANT: If you add or remove a printing function, please mirror
   your changes in the aliases below accordingly. If you export
   functions printing special nodes of the CST, please call them
   "print_<node>_to_<dest>". *)

type ('src, 'dst) printer = state -> 'src -> 'dst

(* Printing nodes *)

val print_to_buffer : (CST.t, Buffer.t) printer
val print_to_string : (CST.t, string) printer

(* Aliases (preferably use fully qualified,
   e.g. [PrintCST.to_buffer]) *)

val to_buffer : (CST.t, Buffer.t) printer
val to_string : (CST.t, string) printer
