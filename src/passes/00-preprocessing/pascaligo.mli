(* Interfacing the preprocessor *)

(* Directories and files *)

type file_path = string
type dirs = file_path list (* #include and #import *)

(* Results *)

module Errors = Preprocessing_shared.Errors

type success = Preprocessor.API.success
type nonrec result  = (success, Errors.t) result

(* Preprocessing various sources *)

val from_file    : file_path option -> dirs -> file_path  -> result
val from_string  : file_path option -> dirs -> string     -> result
val from_channel : file_path option -> dirs -> in_channel -> result

(* Aliases *)

val preprocess_file    : file_path option -> dirs -> file_path  -> result
val preprocess_string  : file_path option -> dirs -> string     -> result
val preprocess_channel : file_path option -> dirs -> in_channel -> result
