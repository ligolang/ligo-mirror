type 'token lex_unit = [
  `Token     of 'token
| `Markup    of Markup.t
| `Directive of Directive.t
]

type 'token t = 'token lex_unit
