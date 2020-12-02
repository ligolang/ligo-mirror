(* Interfacing the preprocessor. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* CONFIGURATION *)

type file_path = string
type dirs = file_path list (* #include and #import *)

module type FILE =
  sig
    include File.S
    val input : file_path option
    val dirs  : dirs
  end

module Config (File : FILE) (Comments : Comments.S) =
  struct
    (* Stubs for the libraries CLIs *)

    module Preprocessor_CLI : Preprocessor.CLI.S =
      struct
        include Comments

        let input     = File.input
        let extension = Some File.extension
        let dirs      = File.dirs
        let show_pp   = false
        let offsets   = true

        type status = [
          `Done
        | `Version      of string
        | `Help         of Buffer.t
        | `CLI          of Buffer.t
        | `SyntaxError  of string
        | `FileNotFound of string
        ]

        let status = `Done
      end

    (* Configurations for the preprocessor based on the
       librairies CLIs. *)

    let preproc =
      object
        method block   = Preprocessor_CLI.block
        method line    = Preprocessor_CLI.line
        method input   = Preprocessor_CLI.input
        method offsets = Preprocessor_CLI.offsets
        method dirs    = Preprocessor_CLI.dirs
      end
  end

(* PREPROCESSING *)

(* Results *)

type success = Preprocessor.API.success
type error   = Errors.preproc_error
type result  = (success, error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
module MakePreproc (File : File.S) (Comments : Comments.S) =
  struct
    (* Postlude *)

    let finalise show_pp = function
      Stdlib.Error (_, msg) -> fail msg
    | Ok (buffer, deps) ->
        let string = Buffer.contents buffer in
        if show_pp then
          Printf.printf "%s\n%!" string;
        Trace.ok (buffer, deps)

    (* Preprocessing a file *)

    let from_file dirs file_path =
      let module File : FILE =
        struct
          let extension = File.extension
          let input     = Some file_path
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let preprocessed =
        Preprocessor.API.from_file Config.preproc file_path in
      finalise Config.Preprocessor_CLI.show_pp preprocessed

    (* Preprocessing a string *)

    let from_string dirs string =
      let module File : FILE =
        struct
          let extension = File.extension
          let input     = None
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let preprocessed =
        Preprocessor.API.from_string Config.preproc string in
      finalise Config.Preprocessor_CLI.show_pp preprocessed

    (* Preprocessing a channel *)

    let from_channel dirs channel =
      let module File : FILE =
        struct
          let extension = File.extension
          let input     = None
          let dirs      = dirs
        end in
      let module Config = Config (File) (Comments) in
      let preprocessed =
        Preprocessor.API.from_channel Config.preproc channel in
      finalise Config.Preprocessor_CLI.show_pp preprocessed
  end
