# The Lexer Library

A lexer is a tool that reads a text file expected to be made of
_lexemes_, and returns abtract versions of them, called _tokens_, for
a parser to consume. The current lexer library supports the making of
UT8-aware lexers for programming languages, like LIGO, by having the
library's client define the tokens as a callback.

This lexer library can also be used to support tools other than
parsers, like pretty-printers and style checkers, as done in the LIGO
compiler suite.

It is designed so it can be used easily with the preprocessor library
also shipped with LIGO.

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
    case,
    [linemarkers](https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html)
    can be found in the input of the lexer.

  * The module `Markup` defines abtract representations for the sorts
    of markup recognised by the lexer: tabulations, blanks, newline
    characters, comments etc.

## Interfaces

### The CLI Interface

The file `CLI.mli` shines a light on the design of `CLI.mli` in the
preprocessor library, in the directory `vendors/Preprocessor`.

First of all, we find the same signature `COMMENTS`:

```
module type COMMENTS =
  sig
    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val block : block_comment option
    val line  : line_comment option
  end
```

It exports optional values denoting two kinds of comments: one-line
comments and block comments (that is, multi-line comments). Due to the
use of metaprogramming by means of `ocamllex` to implement the module
`API`, where comments are defined as regular expressions, the client
of `CLI` must make sure to choose line and block comments that are
actually recognised by `API`.

Next, we found a signature `PREPROCESSOR_CLI` that is equal to `S` in
`CLI.mli` located in `vendors/Preprocessor`. The reason why that
signature is duplicated is because we wanted to avoid as much as
possible dependencies between vendored libraries. Since signature
equality is structural, we can simply duplicate `COMMENTS` and `S`
from the preprocessor library, because the same structural equality
holds fo object types, like `block_comment`. The `CLI` of the lexer
library re-exports the CLI of the preprocessor as module
`Preprocessor_CLI`:

```
module type S =
  sig
    module Preprocessor_CLI : PREPROCESSOR_CLI

    val preprocess : bool

    val mode : [`Byte | `Point]

    val command : [`Copy | `Units | `Tokens] option

    type status = [
      Preprocessor_CLI.status
    | `Conflict of string * string (* Two conflicting options *)
    ]

    val status : status
  end
```

This is a design pattern we will see at work again in the parser
library `ParserLib`:

  1. Include and re-export the previous CLI in the compiler pipeline.

  2. Extend the type `status` with any new error that may arise by
     running this CLI.

  3. Export a value `status` of that type.

  4. Add extra fields (e.g., `preprocess`, `mode`, `command`).

This pattern makes it possible to accumulate CLIs from one pass of the
compiler to the next: the command-line options of the previous stage
are added to the current ones, and only the latter need defining.

If the value `mode` is `` `Byte``, then the unit in which source
positions and regions are expressed in messages is the byte. If ``
`Point``, the unit is unicode points.

The value `command` denotes some possible behaviours of the
compiler. The constructors are

  * `` `Copy``: the lexemes of tokens and markup will be printed to
    standard output, with the expectation of a perfect match with the
    input file;

  * `` `Units``: the tokens and markup will be printed to standard
    output, that is, the abstract representation of the concrete
    lexical syntax;

  * `` `Tokens``: the tokens only will be printed.

### The API Interface

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

This means the following.

  1. Tokens, abstract here, need to be defined by a client module of
     signature `LEXER`. That client is expected to be the client of
     the library itself.

  2. The scanner is defined by the client by means of the module
     `Core`.

In other words, the client defines the tokens and instanciates a
`Core.scanner` (in other words: the client's lexer), then call
`API.Make` for specific uses.

The signature `S` is best construed as a simplification and
specialisation of the interface of `Core`, but `Core` can be accessed
directly for some uses. We shall explain `Core` in section about
[the Core interface](#the-core-interface), and simply assume here that
`Core.scanner` is a low-level lexer returning tokens.

The purpose of the functor `Make` is to take that low-level
representation of a lexer from `LEXER` and produce a high-level module
that enables the library's client to build a standalone lexer or to
integrate it in the LIGO compiler. Otherwise, any client lexer would
use this library through `Core` directly.

Usual lexers are only expected to produce tokens, that is, words in
the input that are relevant to a parser, but this library also
features the production of _lexical units_, which is a superset of
tokens: tokens and markup (see module `Markup`). The rationale for
this unusually rich information is to enable tools other than parsers
to have access to the maximum of details about the input source. Such
a tool could be a pretty-printer, or a style checker, as found in the
LIGO compiler suite (it is not just a compiler).

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
like whether comments are expected and, if so, what kind, also whether
the input was a file or `stdin`, etc. The second parameter is the
source read by the lexer, and the successful result is the type
parameter `'dst`.

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
demand from the lexer. The functions above can be used to obtain a
lexer of that type. The more casual client will best benefit from the
following modules.

The module `Token` in `S` exports the same set of lexers as above, but
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

### The Core Interface

The module `Core` is the heart of the library. Let us walk through the
interface file `Core.mli` and explain some of its values and types,
starting with the types that we saw in the section about
[the API interface](#the-api-interface), that is, `Core.lex_unit`,
`Core.config` and `Core.instance`.

#### Type config

Type `Core.config` gathers information about the input, about what to
do, about how to display error messages, and how to print tokens:

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

  * The value for the field `block` is `None` if the lexer should not
    look for multi-line comments, also called here _block
    comments_. If block comments are to be expected, the type
    `block_comment` gathers how they start and how they close.

  * The field `line` is the equivalent of `block` for one-line
    comments, here called _line comments_, and holds the string that
    act as an opening marker, if any.

  * The value of the field `input` is `None` if the input is
    `stdin`. Otherwise, it holds a filesystem path to the input file.

  * The value for the field `offsets` is `true` to have any error
    message report the location in the inpur source in terms of
    horizontal offset, à la Emacs, instead of column number (`false`,
    like Vim).

  * The field `mode` records whether error messages should report
    locations in the source assuming UTF-8 codepoints or only bytes
    (characters). UTF-8 is only recognised as such in comments, so
    choosing `` `Point`` versus `` `Byte`` has an impact only on
    errors happening after a UTF-8 comment.

  * The field `command` register an optional action to undertake while
    the lexer is running: ``Some `Copy`` will reproduce the input to
    the output from the lexical units, ``Some `Units`` will print out
    the lexical units, and ``Some `Tokens`` will print out only the
    tokens. This is for debugging the lexer.

  * The remaining fields are functions extracting information from
    tokens or converting them, mostly for printing purposes.

#### Type thread

When scanning structured constructs, like strings and comments, we
need to keep the region of the opening symbol, like a double quote for
strings, the marker `//` or `(*`, in order to report any error in them
more precisely. Since `ocamllex` is byte-oriented, we need to store
the parsed bytes as characters in an accumulator so, when are done
scanning the compound, it is easy to build the string making up the
structured construct. The data structure gathering everything needed
for this goal is called here a _thread_ (because it is threaded along
calls):

```
type thread = <
  opening     : Region.t;
  length      : int;
  acc         : char list;
  to_string   : string;
  push_char   : char -> thread;
  push_string : string -> thread;
  set_opening : Region.t -> thread
>
```

The fields are explained as follows:

  * The field `opening` holds the location of the opening marker of a
    string or a comment.

  * The field `length` is the length of the field `acc`.

  * The field `acc` is a stack made of the characters of the string or
    comment being recognised (the characters are therefore reversed).

  * The field `to_string` extracts the string or comment, by relying
    on the fields `acc` and `length`.

  * The methods `push_char` and `push_string` push a recognised
    character, respectively string, onto the accumulator.

  * The method `set_opening` set the field `opening` to a given
    value. It is used when scanning nested comments: when inside a
    comment and finding the start of another one, the opening of the
    current one is saved, the opening of the new one is set with
    `set_opening` and scanning is resumed until the nested comment is
    finished and we can restore the original opening.

#### Type state

Perhaps the more important type of module `Core` is `state`. Beyond
producing lexical units (including tokens), the result of lexing is a
_state_. The type `state` represents the abstract logical state of the
lexing engine, that is, a value which is threaded during scanning and
which denotes useful, high-level information beyond what the type
`Lexing.lexbuf` in the standard library already provides for all
generic lexers. We call it a "logical state" because the lexing buffer
itself has a "physical state" defined by the type `Lexing.lexbuf`.

Tokens are the smallest units used by the parser to build the abstract
syntax tree. Lexical units include tokens, but also markup
(whitespace, comments) and preprocessing directives that were
generated by the preprocessor, like linemarkers after file
inclusions. (Other directives passing through the preprocessor would
mean they are invalid, as the preprocessor does not raise an error in
that case.)

First, we need to explain a couple more of types used by `Core.state`.

We mentioned several times that this lexer library can supply lexical
units, tokens and preprocessing directives (e.g. linemarkers). More
precisely, this means:

```
type 'token lex_unit =
  Token     of 'token
| Markup    of Markup.t
| Directive of Directive.t
```

The type of the tokens is a parameter, because it is defined by the
client of the library.

Then there is the type `window`:

```
type 'token window = <
  last_token    : 'token option;
  current_token : 'token           (* Including EOF *)
>
```

A object of this type is used in case of parse error. When a parser
consumes the tokens produced by the client of this library, it is fed
a window of the last or two last tokens, so the error message is more
precise.

We can now look at the mouthful

```
type 'token state = <
  config       : 'token config;
  window       : 'token window option;
  pos          : Pos.t;
  set_pos      : Pos.t -> 'token state;
  slide_window : 'token -> 'token state;
  sync         : Lexing.lexbuf -> 'token sync;
  decoder      : Uutf.decoder;
  supply       : Bytes.t -> int -> int -> unit;
  mk_line      : thread        -> 'token lex_unit * 'token state;
  mk_block     : thread        -> 'token lex_unit * 'token state;
  mk_newline   : Lexing.lexbuf -> 'token lex_unit * 'token state;
  mk_space     : Lexing.lexbuf -> 'token lex_unit * 'token state;
  mk_tabs      : Lexing.lexbuf -> 'token lex_unit * 'token state;
  mk_bom       : Lexing.lexbuf -> 'token lex_unit * 'token state
>

and 'token sync = {
  region : Region.t;
  lexeme : lexeme;
  state  : 'token state
}
```

The fields and methods are as follows.

  * The field `config` is explained by the section on
    [the type config](#type-config)

  * The field `window` is explained above in this section.

  * The field `pos` is the current position in the file.

  * The method `set_pos` sets the current position in the file. This
    is useful when dealing with linemarkers, or newline characters. In
    other cases, the field `pos` is updated by the method
    `sync`. Indeed, the position is not always updated after a single
    character has been matched: that depends on the regular expression
    that matched the lexing buffer, and whether we decide to perform a
    rollback by calling `rollback` or not.

  * The method `slide_window` updates the field `window` when new
    tokens are recognised.

  * The method `sync` updates the state after a regular expression
    matched. It updates the current position in accordance with the
    contents of the lexing buffer, more precisely, depending on the
    length of the string which has just been recognised by the
    scanner: that length is used as a positive offset to the current
    column. The return type `sync` is a record meant to contain the
    `region` of the lexeme in the input source, the `lexeme` itself
    and the updated `state`. The method `sync` must be called in the
    semantic action with the lexing buffer that was matched by the
    regular expression.

  * The field `decoder` and method `supply` offer the support needed
    for the lexing of UTF-8 encoded characters in comments (the only
    place where they are allowed in our input languages). The former
    is the decoder proper and the latter is the effectful function
    that takes a byte, a start index and a length and feed it to
    `decoder`. See the documentation of the third-party library
    [Uutf](https://github.com/dbuenzli/uutf).

  * The remaining methods have names of starting with `mk_` followed
    by the kind of lexical unit (not a token) they build: `mk_line`
    for line comments, `mk_block` for block comments, `mk_newline` for
    newline characters, `mk_space` for blank space, `mk_tabs` for
    tabulations and `mk_bom` for the UTF-8
    [Byte-Ordered Mark](https://en.wikipedia.org/wiki/Byte_order_mark). Note
    how they all return a new state, besides the lexical unit.

One function which could arguably be part of the type `state` is
`linemarker`:

```
val linemarker :
  Region.t ->
  line:string ->
  file:string ->
  ?flag:char ->
  'token state ->
  Lexing.lexbuf ->
  'token lex_unit * 'token state
```

Just as the methods of `state` whose name start with `mk_`, the
function `linemarker` updates the state after a linemarker has been
recognised, and thus returns a new state. Before describing the
parameters, let us recall that the form of a line marker is:

```
# <line> "<file name>" [<optional 1 or 2>]
```

So, the first parameter of the function `linemarker` is the region of
the entire directive; the second is the `line` specified by the
marker; the third is the `file name` specified by the marker; the
fourth is the optional flag `1` or `2` of the marker; the fifth is the
current state; the last and sixth is the current lexing buffer. The
lexical unit returned is constructed using `Directive`.

Let us continue with the type `scanner`, which is used in the
signature `LEXER` of `API`:

```
type 'token scanner =
  'token state ->
  Lexing.lexbuf ->
  ('token lex_unit * 'token state, message) Stdlib.result
```

A scanner is a bit like a generic `ocamllex` parse rule:

```
rule scanner state = parse
...
```

It takes the current state and lexing buffer. In case of success, a
lexical unit is returned, together with a new state which we can feed
back to the scanner (the lexical buffer is updated effectfully, so
there is no need to return it). Here is again how the type `scanner`
is used by the `API`:

```
module type LEXER =
  sig
    type token

    val scan : token Core.scanner
  end
```

The value `scan` is the fundamental scanner (lexer) from which all
those exported by the signature `S` of `API.Make` are made, that is,
either lexers that return lexical units or tokens one by one, or a
list of them, and this from a variety of sources.

We now move to the client-side of the library by considering the data
structure that the client (that is, the lexer of your programming
language) is expected to provide:

```
type 'token cut =
  thread * 'token state -> 'token lex_unit * 'token state

type 'token client = <
  mk_string                : 'token cut;
  mk_eof                   : 'token scanner;
  callback                 : 'token scanner;
  support_string_delimiter : char -> bool
>
```

The type `cut` is the type of a function that takes a
[thread](#type-thread) and returns the corresponding lexical unit,
that is, either a string or a comment. The state is updated in the
process, so it is given as an argument and the new version is
returned.

The type `client` gathers all the information the client lexer needs
to provide:

  * A method `mk_string` that makes a string from a thread (it must be
    provided by the client because only the client knows the
    implementation of tokens: they are parametric in `Core`).

  * A method `mk_eof` that makes a token for the end-of-file
    characters. Again, only the client lexer knows the implementation
    of the corresponding token.

  * A method `callback` which is a lexer/scanner (both words are
    equivalent in this documentation) for tokens other than strings
    and end-of-file.

  * A method `support_string_delimiter` which is a predicate telling
    whether the character it is given delimits a string. This enables
    to have different string delimiters for different client lexers.

Let us move now to the data structure which represents the
feature-rich, parameterised lexer of `Core`:

```
type input =
  File    of file_path
| String  of string
| Channel of in_channel
| Buffer  of Lexing.lexbuf

type 'token instance = {
  input      : input;
  read_token : Lexing.lexbuf -> ('token, message) result;
  read_unit  : Lexing.lexbuf -> ('token lex_unit, message) result;
  lexbuf     : Lexing.lexbuf;
  close      : unit -> unit;
  window     : unit -> 'token window option
}
```

A look back at `API.mli` is useful here. The functions in the functor
signature `S` are lexers whose destination is a value of type `token
Core.instance`. They are meant to be the most versatile lexers,
whereas the ones made available in the modules `Tokens` and `LexUnits`
are specialised versions, returning lists of tokens or lists of
lexical units, respectively.

The type `input` is straightforward: it informs about the kind of
input. The type `instance` is worth explaining in more details.

  * The field `input` is obvious.

  * The field `read_token` is a lexer that returns one token or an
    error message, almost in the fashion expected by `menhir`.

  * The field `read_unit` is more general than `read_token`, as it
    returns one lexical unit per call, in case of success.

  * The field `lexbuf` is the current lexing buffer.

  * The method `close` closes the input channel, if any (depends on
    the field `input`), to avoid memory leaks.

  * The method `window` returns an option window of the last and maybe
    penultimate token (for error messages).

Finally, the consider the function `open_stream`:

```
val open_stream :
  'token config ->
  scan:('token scanner) ->
  input ->
  ('token instance, message) Stdlib.result
```

This is the function that makes a lexer `instance`. It is only used in
`API`:

```
    let from_file config path =
      Core.open_stream config ~scan:Lexer.scan (Core.File path)
```

We therefore see that it gathers the configuration `config`, the
client's lexer `Lexer.scan`, and the source `Core.File path` in order
to call `open_stream` and produce a lexer instance.

## Implementations

### The CLI Implementation

### The API Implementation

### The Core Implementation
