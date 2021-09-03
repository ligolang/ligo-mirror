(* Preprocessing errors *)

type error =
  Missing_endif
| Dangling_endif
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Missing_space
| Error_directive of string           (* #error ONLY *)
| Invalid_symbol                      (* #define and #undef *)
| File_not_found of string            (* #include #import *)
| Missing_filename                    (* #include #import *)
| Missing_module                      (* #import *)
| Unterminated_comment of string
| Unexpected_argument                 (* #include and #import *)
| Newline_in_string                   (* #include and #import *)
| Unterminated_string of string       (* #include and #import and strings *)
| Invalid_character_in_string of char
| Invalid_character of char           (* #if and #elif *)
| Parse_error                         (* #if and #elif *)

type t = error

val to_string : t -> string
