(* Command-Line Interface (CLI) *)

(* General configuration *)

module type CONFIG = module type of Config

(* CLI options *)

module type OPTIONS = module type of Options

(* Configuration, options and the parsing status of the latter *)

module type PARAMETERS =
  sig
    module Config  : CONFIG
    module Options : OPTIONS

    (* Status after parsing CLI options *)

    type status = [
      `Done
    | `Version      of string
    | `Help         of Buffer.t
    | `CLI          of Buffer.t
    | `SyntaxError  of string
    | `FileNotFound of string
    | `WrongFileExt of string
    ]

    val status : status
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (Config : CONFIG) : PARAMETERS
