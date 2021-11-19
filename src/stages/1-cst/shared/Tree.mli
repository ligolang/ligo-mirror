(* This module is a DSL for building textual representations of
   Catalan trees (general trees). Some functions are specialised for
   LIGO, as they assume that part of their input has type ['a Wrap.t],
   as used in the LIGO CSTs. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

type wrap = string Wrap.t

(* STATE *)

(* The printing of the tree makes use of a threaded data structure:
   the _state_. The printing is done to the string buffer bound to the
   field [buffer], which is imperatively updated (see module
   [Stdlib.Buffer].) The method [pad] updates the current padding,
   which is comprised of two components: the padding to reach the new
   node (space before reaching a subtree, then a vertical bar for it)
   and the padding for the new node itself, determining whether it is
   the last child of its parent. *)

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

val mk_state : ?buffer:Buffer.t -> offsets:bool -> [`Byte | `Point] -> state

(* Printing nodes *)

type 'a printer = state -> 'a -> unit

(* The call [print_node ?region state item] prints a leaf of the tree
   with or without its region in compact form. The label of the leaf
   has type [string]. This fonction can be used when printing nodes
   whose aim is to guide the interpretation, but do not correspond to
   an actual node in the tree, for example "<cst>", or "<statements>"
   (in other word, metadata nodes). *)

val print_node : ?region:Region.t -> string printer

(* The call [print_literal state wrap] prints a leaf for a literal
   [wrap] (therefore the label for the leaf has type [string]). *)

val print_literal : wrap printer

(* The call [print_literal_wo_reg state wrap] is the same as
   [print_literal] but without printing the region. *)

val print_literal_wo_reg : wrap printer

(* Making subtrees (children) from
     * general values ([mk_child]),
     * optional values ([mk_child_opt]) or
     * list values ([mk_child_list]).
   The type of a subtree ("child") is ['a option], with the
   interpretation that [None] means "no subtree printed". In the case
   of a list, the empty list is interpreted as meaning "no subtree
   printed." *)

type child = (state -> unit) option

val mk_child : wrap printer -> wrap -> child

val mk_child_opt : wrap printer -> wrap option -> child

val mk_child_list : (state -> wrap list -> unit) -> wrap list -> child

(* Printing trees (root + subtrees). The call [print_tree ?region
   state label children] prints a root whose label is [label] and
   optional region is [region], and whose subtrees are [children]. The
   latter is a list of optional values, with the interpretation of
   [None] as meaning "no subtree printed". *)

type label = string

val print_tree :
  ?region:Region.t ->
  state ->
  label ->
  (state -> unit) option list -> (* children *)
  unit

(* A special case of tree occurs often: the unary tree made of a value
   of type [wrap], that is, a tree with exactly one subtree. *)

val print_unary :
  ?region:Region.t ->
  state ->
  label ->
  wrap printer (* child printer *) ->
  wrap (* child *) ->
  unit

(* PRINTING TOKENS (LEAVES) *)

type lexeme = string

(* Strings *)

val print_string : wrap printer

(* Verbatim strings *)

val print_verbatim : wrap printer

(* Integers *)

val print_int : label -> (lexeme * Z.t) Wrap.t printer

(* Natural numbers *)

val print_nat : label -> (lexeme * Z.t) Wrap.t printer

(* Bytes *)

val print_bytes : label -> (lexeme * Hex.t) Wrap.t printer

(* Mutez *)

val print_mutez : label -> (lexeme * Int64.t) Wrap.t printer
