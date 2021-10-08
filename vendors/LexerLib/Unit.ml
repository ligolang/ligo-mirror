(* Lexical units *)

type 'token lex_unit = [
  `Token     of 'token
| `Markup    of Markup.t
| `Directive of Directive.t
]

type 'token t = 'token lex_unit

(* Printing *)

type 'token formatter =
  offsets:bool -> [`Byte | `Point] -> 'token -> string

let print_tokens ~offsets mode ~token_to_string buffer lex_unit : unit =
  let output_nl str = Buffer.add_string buffer (str ^ "\n") in
  match lex_unit with
    `Token t -> token_to_string ~offsets mode t |> output_nl
  | `Markup _ | `Directive _ -> () (* Only tokens *)

let print_units ~offsets mode ~token_to_string buffer lex_unit : unit =
  let output_nl str = Buffer.add_string buffer (str ^ "\n") in
  let string =
    match lex_unit with
      `Token t     -> token_to_string ~offsets mode t
    | `Markup m    -> Markup.to_string ~offsets mode m
    | `Directive d -> Directive.to_string ~offsets mode d
  in output_nl string

let print_copy ~token_to_lexeme buffer lex_unit : unit =
  let lexeme =
    match lex_unit with
      `Token t     -> token_to_lexeme t
    | `Markup m    -> Markup.to_lexeme m
    | `Directive d -> Directive.to_lexeme d
  in Buffer.add_string buffer lexeme
