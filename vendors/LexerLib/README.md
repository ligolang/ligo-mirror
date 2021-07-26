# The Lexer Library

A lexer is a tool that reads a text file expected to be made of
lexemes and returns tokens for a parser to consume. The current lexer
library is actually more general than that, as it can provide support
for tools other than parsers, like pretty-printers and style
checkers. We document here the UTF8-aware lexer library shipped with
the LIGO compiler. It is designed so it can be used easily with the
preprocessor library.

## Contents

This directory is located in the `vendors` directory of the repository
of LIGO, here `ligo/vendors`. As such, it is distributed with the LIGO
compiler which uses it as a library. The directory
`ligo/vendors/LexerLib` should list the following files:

```
LexerLib
├── API.ml
├── API.mli
├── CLI.ml
├── CLI.mli
├── Core.mli
├── Core.mll
├── Directive.ml
├── Directive.mli
├── dune
├── dune-project
├── LexerLib.opam
├── LICENSE
├── Markup.ml
├── Markup.mli
└── README.md
```

Here is a short description of those files and OCaml modules:

  * The OCaml module `Core` is the heart of the lexer library. It can
    be considered as the low-level lexer engine, with lots of bells
    and whistles.

  * The module `API` packages types, functions and modules for the
    client, based on `Core`. It is, in a way, a simplified view of
    `Core`, with some specific uses in mind.

  * The module `CLI` deals with command-line options for building a
    standalone lexer, and also export data structures about the
    configuration meant for the library client, for example, the LIGO
    compiler.

  * The `LICENSE` file must contain the MIT license.

  * The file `README.md` is the present file.

  * The module `Directive` defines the abstract representation and
    printers for the preprocessing directives. Indeed, the lexer is
    designed so it can be used with the preprocessor library, in which
    case, linemarkers can be found in the input of the lexer.

  * The module `Markup` defines abtract representations for the sorts
    of markup recognised by the lexer: tabulations, blanks, newline
    characters, comments etc.

## The API Interface

The interface `API.mli` is the best place to start to understand the
architecture and client-side features offered by the lexer
library. It contains three components:

  1. a signature `LEXER`,

  2. a signature `S`,

  3. a functor `Make` from `LEXER` to `S`.

First of all:

```
module type LEXER =
  sig
    type token

    val scan : token Core.scanner
  end
```

This means that _tokens_ are abstract here, and that the lexer comes
from `Core`. This signature is best construed as a simplification of
the interface of `Core`. We shall explain `Core` in section about
[the Core interface](#the-core-interface), and simply assume here that
`scan` is a low-level lexer returning tokens.

The purpose of the functor `Make` is to take that low-level
representation of a lexer from `LEXER` and produce a high-level module
that enables the library's client to build a standalone lexer or to
integrate it in the LIGO compiler. Usual lexers are only expected to
produce tokens, that is, words in the input that are relevant to a
parser, but this library also features the production of _lexical
units_, which is a superset of tokens: tokens and markup (see module
`Markup`). The rationale for this unusually rich information is to
enable tools other than parsers to have access to the maximum of
details about the input source. Such a tool could be a pretty-printer,
or a style checker, as found in the LIGO compiler suite (it is not
just a compiler).

Let us consider the type of lexers, as found in the signature `S` in
`API`:

```
    type ('src,'dst) lexer =
      token Core.config ->
      'src ->
      ('dst, message) Stdlib.result
```

The first parameter of this functional type is `token Core.config`,
that is, an object type gathering generic information about the input,
like if comments are expected and, if so, what kind, also if the input
was a file or `stdin`, etc. The second parameter is the source read by
the lexer, and the successful result is the type parameter `'dst`.

Then follow a series of lexers of type `lexer`, from various inputs to
`token Core.instance`:

```
    val from_lexbuf  : (Lexing.lexbuf, token Core.instance) lexer
    val from_channel : (in_channel,    token Core.instance) lexer
    val from_string  : (string,        token Core.instance) lexer
    val from_buffer  : (Buffer.t,      token Core.instance) lexer
    val from_file    : (file_path,     token Core.instance) lexer
```

The type `Core.instance` will be described in a later section. Suffice
it to say here that it represents a lexer engine, with many
functionalities, rather complex and low-level. In other words, this
part of the signature `S` is for discriminating clients who know what
they are doing. For example, parser generators like `menhir` or
`ocamlyacc` generate a parser type of the form

```
Lexing.lexbuf -> (Lexing.lexbuf -> token) -> tree
```

In other words, it expects a lexer to have type `Lexing.lexbuf ->
token`, because the parser has the control and requests tokens on
demand from the lexer. The functions above can be used in that
way. The more casual client will best benefit from the following
modules.

The module `Token` in `S` exports the same set of lexer as above, but
the destination type is `token list`. In other words, the lexer
completes a full run on its input and, if no error occurred, returns
the list of all tokens.

```
    module Tokens :
      sig
        val from_lexbuf  : (Lexing.lexbuf, token list) lexer
        val from_channel : (in_channel,    token list) lexer
        val from_string  : (string,        token list) lexer
        val from_buffer  : (Buffer.t,      token list) lexer
        val from_file    : (file_path,     token list) lexer
      end
```

This is useful when building standalone lexers, for testing purposes
for instance.

Finally, the module `LexUnits` in `S` is more general than `Tokens`,
in the sense that its lexers return _lexical units_ rather than only
tokens. Lexical units consists in tokens and markup.

```
    module LexUnits :
      sig
        type nonrec 'src lexer = ('src, token Core.lex_unit list) lexer

        val from_lexbuf  : Lexing.lexbuf lexer
        val from_channel : in_channel    lexer
        val from_string  : string        lexer
        val from_buffer  : Buffer.t      lexer
        val from_file    : file_path     lexer
      end
  end
```

The definition of `Core.lex_unit` is found in the next section.

## The Core Interface

Let us walk through the file `Core.mli` and explain some of its values
and types. Let us start with the types of `Core` that we saw in the
section about [the API interface](#the-api-interface), that is,
`Core.lex_unit`, `Core.config` and `Core.instance`.

### Type Core.config

Type `Core.config` gathers information about the input, about what to
do, about how to display error message and how to print tokens.

```
type line_comment  = string (* Opening of a line comment *)
type block_comment = <opening : string; closing : string>

type command = [`Copy | `Units | `Tokens] option

type 'token config = <
  block     : block_comment option;
  line      : line_comment option;
  input     : file_path option;
  offsets   : bool;
  mode      : [`Byte | `Point];
  command   : command;
  is_eof    : 'token -> bool;
  to_region : 'token -> Region.t;
  to_lexeme : 'token -> string;
  to_string : offsets:bool -> [`Byte | `Point] -> 'token -> string
>
```

Let go through all the fields:

  * The field `block` is `None` if the lexer should not look for
    multi-line comments, also called here _block comments_. If block
    comments are to be expected, the type `block_comment` gathers how
    they start and how they close.

  * The field `line` is the equivalent of `block` for one-line
    comments, here called _line comments_.

  * The field `input` is `None` if the input is `stdin`. Otherwise, it
    holds a filesystem path to the input file.

  * The field `offsets` is `true` if
