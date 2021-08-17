(* Interfacing the preprocessor with the LIGO compiler depending on
   the concrete syntax *)

(* Vendors dependencies *)

module Config = Preprocessor.Config
module API    = Preprocessor.API

(* Functor *)

module type S =
  sig
    (* Some inputs *)

    type file_path = string
    type directories = file_path list

    (* Results *)

    module Errors = Errors

    type nonrec result = (API.success, Errors.t) result

    (* Preprocessing various sources *)

    val from_file    : directories -> file_path  -> result
    val from_string  : directories -> string     -> result
    val from_buffer  : directories -> Buffer.t   -> result
    val from_channel : directories -> in_channel -> result

    (* Aliases *)

    val preprocess_file    : directories -> file_path  -> result
    val preprocess_string  : directories -> string     -> result
    val preprocess_buffer  : directories -> Buffer.t   -> result
    val preprocess_channel : directories -> in_channel -> result
  end

module Make (Config : Config.S) : S
