(* The API of the preprocessor

   This preprocessor recognised the following directives:

   "#define", "#elif", "#else", "#endif", "#error", "#if", "#include",
   and "#undef".

   Those are derived from the C# preprocessor.

   Note that unknown directives will be treated like plain text
   instead of raising an error.

   Strings and comments are only recognised in text areas that are to
   be copied. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* In case of success, a buffer containing the preprocessed input is
   returned, together with the list of imported modules and their
   locations on the file system. In case of an error, we return the
   preprocessed buffer so far. *)

type file_path   = string
type module_name = string
type module_deps = (file_path * module_name) list
type success     = Buffer.t * module_deps
type message     = string Region.reg
type error       = Buffer.t option * message
type result      = (success, error) Stdlib.result

type 'src preprocessor = State.config -> 'src -> result

(* Preprocessing from various sources *)

val from_lexbuf  : Lexing.lexbuf preprocessor
val from_channel : in_channel    preprocessor
val from_string  : string        preprocessor
val from_file    : file_path     preprocessor
