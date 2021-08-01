(* Preprocessing errors *)

type error =
  Directive_inside_line
| Missing_endif
| Newline_in_string
| Unterminated_string
| Dangling_endif
| Open_region_in_conditional
| Dangling_endregion
| Conditional_in_region
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Error_directive of string           (* #error ONLY *)
| Parse_error
| Invalid_symbol
| File_not_found of string
| Unterminated_comment of string
| Missing_filename                    (* #include *)
| Unexpected_argument                 (* #include and #import *)

type t = error

val to_string : t -> string
