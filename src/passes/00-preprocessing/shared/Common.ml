(* Interfacing the preprocessor with the LIGO compiler *)

(* Vendors dependencies *)

module Config = Preprocessor.Config
module API    = Preprocessor.API

(* PREPROCESSING *)

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

module Make (Config : Config.S) =
  struct
     (* Some inputs *)

    type file_path = string
    type directories = file_path list

    (* Results *)

    module Errors = Errors

    type nonrec result = (API.success, Errors.t) result

    (* Postlude *)

    let finalise show_pp = function
      Error (_, msg) ->
        Error (Errors.generic msg)
    | Ok (buffer, deps) ->
        if show_pp then
          let string = Buffer.contents buffer
          in Printf.printf "%s\n%!" string
        else ();
        Ok (buffer, deps)

    (* Preprocessing a file *)

    let from_file dirs file_path =
      let module Options =
        struct
          let input   = Some file_path
          let dirs    = dirs
          let show_pp = false
          let offsets = true  (* TODO Flow from the compiler CLI *)
        end in
      let open Preprocessor.API.Make (Config) (Options)
      in finalise Options.show_pp @@ from_file file_path
    let preprocess_file = from_file

    (* Preprocessing a string *)

    let from_string dirs string =
      let module Options =
        struct
          let input   = None
          let dirs    = dirs
          let show_pp = false
          let offsets = true  (* TODO Flow from the compiler CLI *)
        end in
      let open Preprocessor.API.Make (Config) (Options)
      in finalise Options.show_pp @@ from_string string
    let preprocess_string = from_string

    (* Preprocessing a string buffer *)

    let from_buffer dirs buffer = from_string dirs @@ Buffer.contents buffer
    let preprocess_buffer = from_buffer

    (* Preprocessing a channel *)

    let from_channel dirs channel =
      let module Options =
        struct
          let input   = None
          let dirs    = dirs
          let show_pp = false
          let offsets = true  (* TODO Flow from the compiler CLI *)
        end in
      let open Preprocessor.API.Make (Config) (Options)
      in finalise Options.show_pp @@ from_channel channel
    let preprocess_channel = from_channel
  end
