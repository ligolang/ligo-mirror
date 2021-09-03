(* Lexer specification for Michelson, to be processed by [ocamllex]. *)

(* VENDOR DEPENDENCIES *)

module Region = Simple_utils.Region
module Thread = LexerLib.Thread
module State  = LexerLib.State

(* TOKENS *)

type lexeme = string

module type TOKEN =
  sig
    type token
    type t = token

    (* Errors *)

    type int_err = Non_canonical_zero

    type annot_err = Annotation_length of int

    type ident_err =
      Valid_prefix       of Pair.index * Pair.tree
    | Invalid_tree       of Pair.index * char * Pair.tree
    | Truncated_encoding of Pair.index * Pair.child * Pair.tree
    | Missing_break      of int
    | Invalid_identifier

    (* Injections *)

    val mk_string : lexeme -> Region.t -> token
    val mk_bytes  : lexeme -> Region.t -> token
    val mk_int    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
    val mk_annot  : lexeme -> Region.t -> (token, annot_err) result
    val mk_sym    : lexeme -> Region.t -> token
    val eof       : Region.t -> token

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
    val to_region : token -> Region.t

    (* Predicates *)

    val is_string : token -> bool
    val is_bytes  : token -> bool
    val is_int    : token -> bool
    val is_ident  : token -> bool
    val is_annot  : token -> bool
    val is_sym    : token -> bool
    val is_eof    : token -> bool

    val is_string_delimiter : string -> bool
  end

(* The signature of the lexer *)

module type S =
  sig
    type token

    type message = string Region.reg

    type lexer =
      token State.t ->
      Lexing.lexbuf ->
      (token * token State.t, message) Stdlib.result

    val mk_string : Thread.t -> token

    (* Predicates *)

    val is_eof              : token -> bool
    val verbatim_delimiters : string * string
  end

(* The functorised interface *)

module Make (Token : TOKEN) : S with type token = Token.t
