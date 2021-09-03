# A GNU Makefile for OCaml applications

The accompanying GNU Makefile automates the build of small
OCaml projects with minimum user input.

In the tradition of helpful makefiles, it calls a minimum number of
times the compilers and the related tools. It has been designed
primarily as a development tool, not a diagnostic tool, so the build
process is detailed in a concise and precise manner, without
redundancy, and a great amount of efforts has been devoted to clear
warning and error reporting, in particular the first error in a causal
chain is detailed and the subsequent others are skipped while the
compilation goes as far as possible, within reasonable limits, e.g.,
if an interface fails to compile, its corresponding implementation is
ignored entirely.  Uncorrected faults, warnings and ignored source
files are reported again, so the programmer is reminded of fixing
them. Optionally, this makefile may also determine a default linking
order for the object files.

Some classic tools, like `ocamlfind`, `ocamllex`, `menhir` and
`camlp4`, are recognised and automatically leveraged. Command line
options specific to a given tool and a given file can be specified in
tag files (metadata).

GNU Make 4.0 or higher enables parallel builds whilst maintaining the
readability of reporting.

Several executables can be built in the same source directory, but,
currently, all source files must reside in the same directory.

The makefile has no interface with control version systems (except
Git, in a commented out section), but it detects modifications,
deletions and creations of source files between build cycles, and it
reacts appropriately so there is no need to restart a build cycle from
a clean slate to correct an inconsistency. Note that, as usual with
makefiles, this build system relies only on time stamps to drive its
actions.

## Configuration

The first action to undertake is to check the availability of the
utilities required to build your application. This is simply achieved
by running

    $ make config

Note that installing `ocamlfind` is highly recommended, so third-party
libraries can be automatically handled by the makefile.

The minimal OCaml application is made up of exactly one implementation
(`.ml`, `.mll` or `.mly`), say `foo.ml`. In the most general case,
several applications can be built from the same source directory.

Each OCaml application can be built in two flavours: bytecode or
native code. The name of the executable will always be the name of the
main implementation, here `foo`, followed by an extension `.byte`
(bytecode) or `.opt` (native code), hence either `foo.byte` or
`foo.opt`.

If there is more than one implementation (`.ml`) to compile, the
executables (perhaps there is only one) must have an accompanying tag
file, possibly empty, which is a hidden file giving options to the
linker. For instance, if the executable is `foo.byte` or `foo.opt`,
then the associated tag file is named `.foo.tag`. This enables the
build of several applications from the same source directory: the
makefile simply determines those applications by identifying those tag
files.

The parser specifications (e.g., `foo.mly`) from which Menhir is
expected to produce a parser require a tag file (`.foo.mly.tag`),
except if there is only one. Specifications not meant to produce a
parser must not have a tag file (for instance, they may be included by
Menhir in another specification).

Optional variables can be set in a file `Makefile.cfg`, amongst the
following.

* `OCAMLC`
  The bytecode compiler. If `ocamlfind` is installed, the default
  value is overriden to "ocamlc", as `ocamlfind` actually takes care
  of calling `ocamlc.opt` instead when available; otherwise the
  default value is "ocamlc.opt" if found, else "ocamlc". The setting
  can be checked by running

    $ make env

  Optionally, you may set `OCAMLC` in `Makefile.cfg`.

* `OCAMLOPT`
  Same as `OCAMLC`, except with "ocamlopt" instead of "ocamlc".

* `OCAMLDEP`
  Same as `OCAMLC`, except with "ocamldep" instead of "ocamlc".

* `OCAMLLEX`
  The standard lexer generator. If `ocamllex.opt` is installed, the
  default value is "ocamllex.opt", else "ocamllex". The setting
  can be checked by running

    $ make env
  Optionally, you may set `OCAMLLEX` in `Makefile.cfg`.

* `VERB`
  If you want a more verbose output, just set it to "yes".

* `DEBUG`
  You may set this variable to "yes" if you want to debug the
  makefile. Note that this implies that `VERB` is set to "yes" as well.

* `SHELL`
  You may set this variable to the command shell you use. Our scripts
  have been tested with `dash` and `bash`. We recommend setting it to
  "dash", if available, for faster builds.

* `BFLAGS`
  Additional flags to the invocation `ocamlc -c` or `ocamlc.opt -c`,
  applying to all OCaml compilation units. (None by default.) Flags
  specific to a unit *mod*`.mli` or *mod*`.ml` must be set in the
  corresponding tag file `.`*mod*`.mli.tag` or `.`*mod*`.ml.tag`.
  See section TAGS below.

* `CFLAGS`
  Additional flags to the invocation `ocamlc -c` or `ocamlc.opt -c` on
  C files. (None by default.)

* `OFLAGS`
  Additional flags to the call `ocamlopt -c` or `ocamlopt.opt -c`,
  applying to all OCaml compilation units. By default, it equals
  ${BFLAGS}, otherwise, file-specific options should be defined in the
  tag file (see `BFLAGS`).

* `DFLAGS`
  Additional flags to the call `ocamldep -modules -one-line` or
  `ocamldep.opt -modules -one-line`, applying to all compilation
  units. (None by default.) File-specific options should be defined in
  the tag file (see `BFLAGS`).

* `LFLAGS`
  Flags to the call `ocamllex` or `ocamllex.opt`, applying to all
  lexer specifications. (None by default.) File-specific options
  should be defined in the tag file `.`*mod*`.mll.tag`, without any tool
  prefix. See section TAGS below.

* `YFLAGS`
  Additional flags to the invocation `menhir -v`, applying to all
  files. (None by default.) File-specific options are in the tag file
  `.`*mod*`.mly.tag`, without any "`menhir: `" prefix.


## The Build directory

The object codes, the executables and metadata are generated in a
subdirectory of the source directory. The name of that directory can
be set in `Makefile.cfg` (variable `OBJDIR`), or automatically
determined by the makefile, in which case it consists of an underscore
followed by the result of the `arch` shell command, e.g., `_i686` or
`_x86_64`.  This naming convention enables multi-architecture
builds. For instance, if the programmer wants the build directory to
be `_build`, they should set it in `Makefile.cfg` as follows:

    OBJDIR := _build


## Targets

To avoid unexpected feature interactions and breaking some GNU Make
assumptions, always use _only one_ of the following targets at each
invocation of GNU Make.

* `config`
   To check the installed OCaml system.

* `env`
   To check the value of some variables denoting OCaml tools.

* `all`, `byte`
   The default target. To update all bytecode executables. Note that
   the executables are not standalone and require the virtual machine
   `ocamlrun`.

* `nat`
   To update all native-code executables, whilst maximising
   independent compilations and minimising opportunities for
   cross-module optimisations, like inlining. For reversed
   constraints, see target `opt`.

 * `opt`
   To update all native-code executables, whilst minimising
   independent compilations and maximising opportunities for
   cross-module optimisations like inlining. (For reversed
   constraints, see target `nat` and section TAGS.) To benefit fully of this
   build, typically for a release, you should make a clean slate with
   `make clean` and then `make opt`. Note that if you update a
   specific object, e.g.,

        $ make parser.cmx

   the compilation dependencies which are automatically chosen are those which
   maximise independent compilations, as with target `nat`.

 * `warn`
   To print the names of the files containing warnings.

 * `clean`
   To delete all the files produced by the OCaml system and the
   makefile.

 * `mostlyclean`
   Same as the target `clean`, except that no executable is deleted.

 * `lines`
   To display the number of OCaml lines of the project. The first
   column contains the number of non-empty, non-comment lines in
   implementations (`.ml`) only. Number of lines with a single ";;" is
   also printed. Requires `ocamlwc`.

Miscellanea targets

 * `size`
   To display the number of lines of the file `Makefile` without comments nor
   blank lines.

 * `dep`
   To update the compilation dependencies when maximising independent
   compilations. For debugging.

 * `phony`
   To print all phony targets (`.PHONY`). For debugging.


## Tags

Specific command-line options to be passed to OCaml tools when
processing a given file *mod*`.mli` or *mod*`.ml` must be written in a
hidden metadata file whose name is `.`*mod*`.mli.tag` or
`.`*mod*`.ml.tag`. The name of the tool must be written at the start
of a line, immediately followed by a semicolon, then the
options. Currently, `ocamldep`, `ocamlc`, `ocamlopt`, `ocamllex` and
Menhir are recognised; for example, if `camlp4o` is needed to compile
the unit *mod*.`ml`, then `.`*mod*`.ml.tag` should contain the lines

    ocamldep: -pp camlp4o
    ocamlc:   -pp camlp4o -w -26
    ocamlopt: -pp camlp4o

(If `ocamlc.opt` and `ocamlopt.opt` are actually the tools being used,
the expected names in the tag file are nevertheless `ocamlc` and
`ocamlopt`, because they accept the same options.) In the absence of a
line about `ocamlopt`, the options for `ocamlc`, if any, are used
instead, therefore, in order to ensure that no options are passed to
`ocamlopt` when options are passed to `ocamlc`, simply write

    ocamlopt:

The options for `ocamllex` must be written in `.`*mod*`.mll.tag`,
those for Menhir in `.`*mod*`.mly.tag`, and those for the linker in
`.`*main*`.tag` or `.`*main*`.opt.tag`, depending on the output code,
without the linker's name (just the options as above).

The modification, deletion or creation of a tag file will entail the
same consequences as if the corresponding source file had been
modified.

If `ocamlfind` is available, you can set the command line options for
it under the driven tool's name, as usual. For example, a tag file
assuming `ocamlfind` and `camlp4` may be

    ocamldep: -syntax camlp4o
    ocamlc:   -syntax camlp4o

It is also possible to specify in the tag file of an executable (used
by the linker) the basenames of the source files containing the
bytecodes to be linked, ordered as the linker expects them to be. The
line must start with

    objects:

and then lists the object basenames, separated by spaces. Doing so
increases speed, and not doing so might lead to unexpected behaviours
in case of interfering side-effects at link-time. Also, it is
necessary in the case of a valid circular dependency, whereby, give
two modules with interfaces and implementations, one implementation
uses a type from the other module, whose implementation uses a value
from the former. We recommend not to specify the objects during
development, not to rely on side-effects when initialising modules,
but to explicitly state the objects in the tag file when releasing the
software, for faster and more predictable builds at the customer's
site.

## Third party libraries

Let us imagine that you want to use Ulex, a library written by Alain
Frisch to parse UTF-8 encoded Unicode points. It is convenient to use
`ocamlfind` (FindLib) to install and manage that library, and also
drive the compilers and related tools. If the makefile finds
`ocamlfind`, then it will be used to drive the OCaml compilers.

Ulex relies on `camlp4` to extend the syntax of OCaml in order to
embed regular expressions in OCaml programs. Let us assume that a
Ulex-based scanner is defined in the file `scan.ml`. Then, you need
the associated tag file `.scan.ml.tag` to contain

    ocamldep: -syntax camlp4o
    ocamlc:   -syntax camlp4o

Note that the Makefile will automatically determine whether Findlib
has installed the package `ulex`, which can then be retrieved by
`ocamlfind`, and it will further silently pass to the compilers the
additional flag `-package ulex`. Similarly, the Makefile will
automatically pass to the linker the option `-package ulex
-linkpkg`. (Note: In case you need additional options for the linker,
put them in the tag file `.scan.tag`, where `Scan` is the main module,
that is, the last to be linked.)


## Linking with C bindings

Let us assume that you want to build your executable with C
bindings. For instance, you have a file named `foo_binding.c`, which
defines a C function whose prototype is `void foo ()`. You mean to
bind this function to your OCaml application through a module `Foo`. You
write, in `foo.mli`:

    val foo : unit -> unit

and, in `foo.ml`:

    external foo : unit -> unit = "foo"

The makefile will automatically detect all C files in the source
directory and compile them in the build directory. (See the `CFLAGS`
variable above.)

You only need now to inform the C linker used by OCaml to include
`foo_binding.o` by writing a tag file `.<main>.tag` like so:

    ocamlc: -custom -cclib foo_binding.o
    ocamlopt: -cclib foo_binding.o

Now you can build and use 'Foo.foo' in your OCaml program.

Note: Make sure that you do not have 'foo.c' and 'foo.ml' in the same
source directory, as this will break any native build. Use
instead the convention 'foo_binding.c' and 'foo.ml'.

## Limitations

During the first build, all source files are built (if generated from
a specification) and parsed with 'ocamldep' or 'ocamldep.opt' to
extract their compilation dependencies. This may that time if there
are many large compilation units. Nevertheless, subsequent extractions
will be based solely on source and tag changes or creation, so the
cost of parsing unused files when building from a clean slate is
likely to be amortised in the long run.


## Session samples

    $ make
    Making lexer.ml from lexer.mll (144 lines)... done:
    54 states, 556 transitions, table size 2548 bytes.
    Making parser.ml(i) from parser.mly (227 lines)... done:
    Built an LR(0) automaton with 183 states.
    Built an LR(1) automaton with 183 states.
    Compiling parser.mli... done (58 lines).
    Compiling lexer.mli... done (19 lines).
    Compiling toppar.ml... done (15 lines).
    Updating linking order... done.
    Compiling dict.mli... done (109 lines).
    Compiling lexis.mli... done (12 lines).
    Compiling dict.ml... done (58 lines).
    Compiling lexis.ml... done (228 lines).
    Compiling lexer.ml... done (515 lines).
    Compiling parser.ml... done (4656 lines).
    Linking objects as _i686/toppar... done.
    > In directory _x86_64, check warnings:
    .lexer.ml.wrn
    .lexis.ml.wrn

    $ make
    Making parser.ml(i) from parser.mly (227 lines)... FAILED:
    File "parser.mly", line 1, characters 2-2:
    Error: unbalanced opening brace.
    Ignoring lexer.mli.
    Ignoring toppar.ml.
    Updating linking order... done.
    Cannot link objects.

    $ make
    Making lexer.ml from lexer.mll (144 lines)... done:
    54 states, 556 transitions, table size 2548 bytes.
    Making parser.ml(i) from parser.mly (227 lines)... done:
    Built an LR(0) automaton with 183 states.
    Built an LR(1) automaton with 183 states.
    Compiling parser.mli... done (58 lines).
    Compiling lexer.mli... done (19 lines).
    Compiling toppar.ml... done (15 lines).
    Updating linking order... done.
    Compiling lexis.mli... done (12 lines).
    Compiling lexer.ml... FAILED:
    File "lexer.mll", line 39, characters 33-43:
    Warning 3: deprecated: String.set
    Use Bytes.set instead.
    File "lexer.mll", line 143, characters 43-44:
    Error: This expression has type int but an expression was expected of type
           unit
    Compiling lexis.ml... done (228 lines).
    Compiling parser.ml... done (4656 lines).
    Cannot link objects.
    > In directory _x86_64, check warning .lexis.ml.wrn


## Build cycle

Source files should reside in the same directory where the makefile is
located.

Before attempting the first build, it is advised to run

    $ make conf
    Found /opt/local/bin/ocamlc
    Found /opt/local/bin/ocamlc.opt
    Found /opt/local/bin/ocamlopt
    Found /opt/local/bin/ocamlopt.opt
    Found /opt/local/bin/ocamldep
    Found /opt/local/bin/ocamldep.opt
    Found /opt/local/bin/ocamllex
    Found /opt/local/bin/ocamllex.opt
    Found /Users/rinderkn/.opam/4.03.0/bin/menhir
    Found /Users/rinderkn/.opam/4.03.0/bin/ocamlfind
    Found /opt/local/bin/ocamlobjinfo
    Found /Users/rinderkn/.opam/4.03.0/bin/ocamlwc
    Found /Users/rinderkn/.opam/4.03.0/bin/camlp4
    Found /opt/local/bin/grep
    Found /opt/local/libexec/gnubin/sed
    Found /usr/bin/arch

to check the availability of the main tools required, in particular,
the presence of `ocamldep.opt`, `ocamlc.opt`, `ocamlopt.opt` or
`ocamllex.opt` may be leveraged by setting appropriate variables in
the configuration file `Makefile.cfg`. In the following, we will write
"ocamllex" to denote either the executable `ocamllex` or
`ocamllex.opt`, and similarly for "ocamlc", "ocamldep" etc.

Let us assume that there is at least a main module, either of the form
*main*`.ml`, *main*`.mll` (lexer) or *main*`.mly` (parser). A build
cycle always starts with the updating of the source files, that is,
generating any lexer by ocamllex and any parser by Menhir. Then
ocamldep updates the compilation dependencies of all source files
present in the source directory. When starting from a clean slate,
this first step is time-consuming and may report syntax errors in
files which are not actually needed to build the
executable. Nevertheless, the updating of dependencies is local, that
is, dependencies are updated if, and only if, the corresponding source
has changed or was created since the last update, so the initial cost
is amortised in the long run. To reduce the noise at this stage,
useless modules should be put in a subdirectory, since the makefile
only considers the current directory when fetching source files.

After updating compilation dependencies, the makefile jumps to the
build directory, which is a sub-directory of the source directory,
which is also the directory where the Makefile resides. The name of
the build directory can be set in Makefile.cfg, for example, if
`_build` is the desired name, we have:

    OBJDIR := _build

Otherwise, the makefile will automatically determine a name, based on
the computer architecture, for instance, `_i686` or `_x86_64`.

Once the build directory is determined and created if necessary, the
makefile jumps into it and the binaries are built there.

Next, two different continuations are possible, depending on the
variable `OBJ` being defined or not in the configuration makefile
`Makefile.cfg`, which is included in the Makefile.

If set, the variable `OBJ` must list the basenames of the object files
in the order expected by the linker, and this order is used to compile
the implementations as well.

If `OBJ` is undefined, *main*`.cmo` or *main*`.cmx` is updated, and so
are the interfaces they depend upon, as well as the implementations
without interfaces they also rely on. Next, the linking dependencies
are determined, based on the compilation dependencies. In the absence
of any compilation error, the objects are linked into the executable
*main* or *main*`.opt`, depending on the target code. Even in the
absence of compilation errors, it is possible that the proper linking
order cannot be determined automatically, in particular when *a*`.ml`
depends on *b*`.mli` and *b*`.ml` depends on *a*`.mli` (valid if at
least one dependency is on a type), in which case the programmer is
informed of a circular dependency and is expected to set `OBJ` in
`Makefile.cfg`. Note that, in any case, setting `OBJ` significantly
speeds up the build cycle.

There are two kinds of compilation to native code: either independent
compilations are maximised (phony target `nat`), or else opportunities
for cross-module optimisations are maximised (phony target
`opt`). Note that it is possible to switch between bytecode and native
code compilation seamlessly, but if the aim is to maximise
cross-module optimisations for the whole project, and `nat` has
already been updated, it is best to restart from a clean slate by
updating the phony target `clean`.


## Error handling

Errors may occur at each stage of a build cycle, entailing two kinds
of consequences: an error may require reporting or some compilation
may be predicted to fail because of a dependency on a faulty
module. If a run of the compiler results in an error, the
corresponding message is logged, so it may be redisplayed when a
recompilation is later requested and is bound to fail again, except if
a dependency is transitively erroneous, in which case the message is
not redisplayed and the compilation is simply skipped and the
programmer informed of this fact.

As mentioned earlier, compilation dependencies are determined by
ocamldep, and the errors it can report are only syntactical in
nature. If a module contains a syntax error, the corresponding message
issued by ocamldep is printed and the module will not be compiled at a
later stage.

Object codes are linked into the executable only in the absence of any
syntactic (always reported by ocamldep) or semantic (compile-time)
error. Here are some of the metadata (hidden files written in the
build directory) related to errors:

  1. The files `.`*mod*`.mli.syn` and `.`*mod*`.ml.syn` contain syntax
     error messages about *mod*`.mli` and *mod*`.ml`, respectively.

  2. The files `.`*mod*`.mli.sem` and `.`*mod*`.ml.sem` contain
     semantic error messages about *mod*`.mli and *mod*`.ml`,
     respectively.

  3. The files `.`*mod*`.mli.ign` and `.`*mod*`.ml.ign` are empty
     files indicating that *mod*`.mli` and *mod*`.ml`, respectively,
     should be ignored and not compiled. Note that, whilst the
     presence of a hidden file of extenstion `.syn` or `.sem` implies
     the existence of a corresponding `.ign` file, the contrary does
     not always hold, as a file may be ignored because its compilation
     would be bound to fail due to a transitive dependency on a faulty
     module.

  4. The files `.`*mod*`.mll.err` and `.`*mod*`.mly.err` contain an
     error message from ocamllex about *mod*`.mll` and an error
     message from Menhir about *mod*`.mly`, respectively.

  5. The files `.`*mod*`.err` and `.`*mod*`.opt.err` contain error
     messages from the linker (respectively, in bytecode and native
     code).

  6. The files `.`*mod*`.mll.err.dis` and `.`*mod*`.mly.err.dis` are
     empty files denoting the fact that the error messages in
     `.`*mod*`.mll.err` and `.`*mod*`.mly.err` have been displayed
     just after *mod*`.ml` and/or *mod*`.mli` failed to be correctly
     generated (they have been replaced by empty stubs).

  7. The files `.`*mod*`.ml.syn.dis` and `.`*mod*`.mli.syn.dis` are
     empty files which, if present, mean that the syntax error message
     (issued by ocamldep) in `.`*mod*`.ml.syn` quiet or
     `.`*mod*`.mli.syn` has already been printed in the current
     session.

  8. The files `.`*mod*`.ml.sem.dis` and `.`*mod*`.mli.sem.dis` are
     empty files which, if present, mean that the semantic error
     message (issued by ocamlc) in `.`*mod*`.ml.sem` quiet or
     `.`*mod*`.mli.sem` has already been printed in the current
     session.

Under no circumstances is the programmer to edit these metadata, or
inconsistency may ensue.

Note that these files may have backup versions, with their original
name extended by `.old`. These files are deleted at the end of a build
cycle, except in debugging mode.


## Deletions

Changes on the code base between two build cycles are of three kinds:
creation, deletion and modification. Deletion often results in
inconsistencies with build systems relying on time stamps provided by
the file system because it breaks the invariant that a file gets older
as time goes by. To avoid any inconsistency due to deletions, this
makefile keeps a record of the source deletions between cycles, as
well as lexer and parser specifications and tag files, so there is no
need to rebuild from a clean slate. Manual editions of the metadata
are not expected, so they must not be attempted (time stamps and/or
contents do matter). On the other hand, manual modifications,
including deletions, of object files are taken care of, but these
should not happen in general and are best avoided since, for instance,
the makefile ensures that the deletion of *mod*`.ml` entails the
automatic deletion of *mod*`.cmx` before compilation (this is
necessary when switching to native code with cross-module
optimisations). Moreover, the metadata associated with *mod*`.ml`
should be deleted, except the tag file `.`*mod*`.ml.tag`, which is
written by the programmer. The general rule of thumb is that the
programmer should not edit generated files and the makefile does
neither edit the files edited by the programmer nor touch their time
stamps. Support for deletions of object files is only meant to enable
workarounds for the programmer who runs afoul of a bug in the
makefile. The support for the deletion of generated lexers and parsers
is meant to allow the programmer to experiment with them.

In order to keep track of deletions, the makefile manages a hidden
file named `.src` containing the source filenames (plus tags and
specifications) during the last build cycle. As a side note, the list
is sorted, but this is only a side-effect of ensuring the uniqueness
of the names in it.


### Creations

The creation of files can be problematic for a Makefile if compilation
dependencies on these files already existed. This happens when
forgetting to import into the source directory some compilation units
which are depended upon: the compiler will still report errors about
missing dependencies. The problem is that, after adding the forgotten
units, these messages will remain because the source where they
originate have not changed, and will not be recompiled.

The solution we propose here is simple. We maintain metadata in a
hidden file `.ext`, made of missing reverse dependencies.

Here is a real use case. Suppose we have imported a set of OCaml files
and compiled them as follows:

    $ make
    Making escan.ml from escan.mll (95 lines)... done:
    59 states, 1786 transitions, table size 7498 bytes.
    Making eparser.ml(i) from eparser.mly (50 lines)... done:
    Built an LR(0) automaton with 24 states.
    Built an LR(1) automaton with 24 states.
    Making preproc.ml from preproc.mll (491 lines)... done:
    159 states, 3017 transitions, table size 13022 bytes
    1993 additional bytes used for bindings.
    Compiling eparser.mli... FAILED:
    File "eparser.mli", line 19, characters 65-72:
    Error: Unbound module Etree
    Ignoring escan.ml.
    Ignoring preproc.ml.
    Ignoring topproc.ml.
    Updating linking order... done.
    Cannot link objects.

At this point we realise that we forgot to import `etree.ml`. If we
look at the `.ext` file, we see the following:

    $ cat .ext
    Array: preproc.ml
    Array: topexp.ml
    Array: topproc.ml
    Bytes: preproc.ml
    Char: escan.ml
    Error: escan.ml
    Error: preproc.ml
    Error: topexp.ml
    Etree: eparser.ml
    Etree: eparser.mli
    Etree: preproc.ml
    Lexing: eparser.ml
    Lexing: eparser.mli
    Lexing: escan.ml
    Lexing: preproc.ml
    Lexing: topexp.ml
    List: preproc.ml
    Obj: eparser.ml
    Pervasives: eparser.ml
    Printf: eparser.ml
    Set: preproc.ml
    String: escan.ml
    String: preproc.ml
    Sys: topexp.ml
    Sys: topproc.ml

Each line is made of a reverse dependency, that is, on the left-hand
side, we have a module depended upon by the right-hand side. We call
this *reverse dependency* because it is the opposite order of a Make
dependency. Some of these external modules on the left-hand sides are
missing for a good reason: they belong to the standard library. (The
reason for keeping dependencies on libraries is that we want to allow
the programmer to shadow them, that is, write their own version that
hides the standard one --- even if this is frowned upon.) The same
could be said of third-party libraries, like those managed by Findlib
(`ocamlfind`) for example. Some other modules are missing when they
should not, like `Etree` and `Error`. Therefore, we import them, say,
from the upper directory:

    $ cp ../error.ml ../etree.ml .

The expected behaviour of the makefile is shown when updating the
byte-code target:

    $ make
    Compiling etree.ml... done (22 lines).
    Compiling error.ml... done (28 lines).
    Compiling eparser.mli... done (20 lines).
    Compiling escan.ml... done (659 lines).
    Compiling preproc.ml... done (1563 lines).
    Compiling topproc.ml... done (3 lines).
    Updating linking order... done.
    Compiling eparser.ml... done (589 lines).
    Linking objects as _i686/topproc... done.

We have now:

    $ cat .ext
    Array: preproc.ml
    Array: topexp.ml
    Array: topproc.ml
    Bytes: preproc.ml
    Char: escan.ml
    Lexing: eparser.ml
    Lexing: eparser.mli
    Lexing: error.ml
    Lexing: escan.ml
    Lexing: preproc.ml
    Lexing: topexp.ml
    List: preproc.ml
    Obj: eparser.ml
    Pervasives: eparser.ml
    Printf: eparser.ml
    Set: preproc.ml
    String: escan.ml
    String: preproc.ml
    Sys: topexp.ml
    Sys: topproc.ml

Note that lines starting with `Error` and `Etree` have been
removed. There has been no need for a clean slate thanks to the
metadata in `.ext`.


## Compilation dependencies

A run of ocamldep on an interface *mod*`.mli` yields the dependencies
for the object file *mod*`.cmi`. From an implementation *mod*`.ml`,
the dependencies of *mod*`.cmo` and *mod*`.cmx` are produced. For the
`.cmx` objects, there exists two kinds of dependencies: if independent
compilations are maximised, then these dependencies are exactly those
of the corresponding `.cmo` objects, that is, only `.cmi` objects;
otherwise, if opportunities for cross-module optimisations are
maximised, dependencies on other modules are based on their `.cmx`,
not their `.cmi` (to enable cross-module inlining, for instance).

### Issues

By default, an object file *foo*`.cmo` depends on *bar*`.cmi` if
*bar*`.mli` exists, and on *bar*`.cmo` when *bar*`.mli` does not
exist. This leads to a problem when an implementation *mod*`.ml`
without an interface, what we call a *standalone implementation*, is
compiled successfully, and then an interface *mod*`.mli` is created:
only the interface would then be compiled, although the implementation
should be recompiled as well, in order to check for inconsistencies
with respect to its newly created interface.

The traditional remedy consists in recomputing the dependencies for
*all* source files, so their dependencies on *mod*`.cmo` are changed
into dependencies on *mod*`.cmi`, but this is slow because it requires
a clean slate.

Another issue is that, also by default, ocamldep ignores any
dependency on *bar*`.cmi` when *bar*`.mli` does not exist: this yields
a problem when a first cycle successfully builds the executable and
then an interface without implementation is deleted: no recompilation
is triggered, whereas the build should be retried and fail. This
default behaviour of ocamldep has also an unfortunate consequence when
dependencies upon a lexer generated by ocamllex or a parser generated
by Menhir are needed: these have to be generated, otherwise their
absence would result in no dependency upon them.

Another issue is the behaviour of ocamldep in the presence of
expressions like

    Sym.(IntSet.elements (get_all def))

in a unit *foo*`.ml`, and where the module `Sym` has a submodule
`IntSet` and a function `get_all`. Then, ocamldep generates a
dependency on both modules, instead of `Sym` alone. Consequently, an
entry

    IntSet: foo.ml

is introduced in the file `.ext`. That entry will be ignored if there
is no module `IntSet` in the OCaml installation managed by Findlib
(`ocamlfind`). Otherwise, a benign capture would occur, e.g., if we
had had

    Sym.(Unix.elements (get_all def))

then the corresponding entry would be

    Unix: foo.ml

which would lead to `unix.cma` or `unix.cmxa` being used by the
compiler to compile *foo*`.ml`, even if it is not actually
required. This would be harmless.

### Solutions

To solve the first problem, we *always* have a file `.cmo` depend on
files `.cmi`. This entails a special treatment of what we call the
*virtual interfaces* of the standalone implementations, but dependency
recomputations then become local, that is, the dependencies of a
compilation unit are remade if, and only if, its source has changed.

To solve the second problem, we run ocamldep with the command-line
option `-modules` and we perform some postprocessing on the output,
for example, to remove modules not found in the source directory.

The dependencies of an implementation *mod*`.ml` are stored in the
hidden file `.`*mod*`.ml.dep`, in the build directory, and that of an
interface *mod*`.mli` in `.`*mod*`.mli.dep`. Note that dependency
files are never empty.

A run of ocamldep may find syntactic errors, but never semantic
errors. Syntax error messages about *mod*`.mli` and *mod*`.ml` are
stored in the hidden files `.`*mod*`.mli.syn` and `.`*mod*`.ml.syn`,
respectively. As a design principle, let us keep in mind that the
updating of compilation dependencies may create or remove files
`.dep`, `.syn` and `.ign`, at the exclusion of any other.

### Macro `mk_dep`

In case ocamldep finds a syntax error, the associated message is
logged in `.`*mod*`.mli.syn` or `.`*mod*`.ml.syn`, and sent to the
terminal. Next, the default dependencies which have been generated are
erased, and, finally, an empty file `.`*mod*`.mli.ign` or
`.`*mod*`.ml.ign` is created to signify that *mod*`.mli` or *mod*`.ml`
has to be ignored by any other units which may depend upon it.

The macro starts by detecting and handling the case of modules without
interfaces (*standalone implementations*), because of the Makefile
rule of attaching then all metadata to a ficticious
interface. Otherwise, the metadata would be missed when making out the
dependencies of an implementation if that implementation stands alone.

It proceeds by detecting whether a syntax error was logged during the
previous build cycle. If so, it checks if the associated source code
and tag file, if any, has been updated since: if so, the dependencies
need to be recomputed, otherwise the logged error message is simply
redisplayed.

Note the use of the Unix utility `find` to compare time stamps,
because `test` *x* `-nt` *y* does not always work when the two time
stamps are very close.

If no update is found to be needed (variable `up` set to `"no"`) because
a syntax error was produced earlier, then we decide whether we have to
print the associated message. If it was displayed earlier in the same
build, the metadata file `.`*mod*`.ml.syn.dis` or `.`*mod*`.ml.syn.dis`
should exist and nothing is done (except in verbose mode, where an
explicit message acknowledges that the file is ignored); otherwise, it
is displayed and the corresponding metadata is touched. The presence
or absence of the files enable the handling of a session for syntax
error reporting, that is, a knowledge that spans an entire build, even
when building multiple targets depending on the same (syntactically)
erroneous modules.

Any empty `.`*mod*`.mli.syn` or `.`*mod*`.ml.syn` file produced by
ocamldep is deleted because this means that no error was actually
found. If *mod*`.mli` or *mod*`.ml` is not empty, that is, it is not a
stub for lexers or parsers for the purpose of dependency generation
(see below section *Generated lexers and parsers (metaprograming)*),
then we must remove `.`*mod*`.mli.ign` and/or `.`*mod*`.ml.ign`
because no syntax error has been found and we must enable their
(re)compilation.

(The following is a side note. As expected, ocamldep issues a
dependency on *mod*`.cmi` for *mod*`.cmx` when it finds *mod*`.mli` in
the source directory, but not *mod*`.ml`. Unfortunately, this
behaviour is the same if *mod*`.ml` is present in another directory
*dir* whose path is given through the `-I` *dir* command line option,
instead of the correct dependency on *dir*`/`*mod*`.cmx`.)

If no error has been detected so far, the macro ocamldep retains
from the dependencies only those that correspond to an actual file,
either directly or indirectly, that is, generated either by ocamllex
or Menhir. Some care is needed so that the script file works on
case-preserving (writes), case-insensitive (reads) operating systems,
like OS X. Furthermore, missing dependencies are stored in a hidden
file `.ext`, used to detect the creation (or import) of files, so that
no clean slate is necessary (see above section *Creations*).

If Findlib (`ocamlfind`) is available, the macro extracts the library
(package) dependencies from the computed module dependencies. (See how
the shell variable `packages` is set.)

The output of ocamldep is further filtered to add the `.cmi`
extensions and ensure that the `.cmo` and `.cmx` files have the same
dependencies, that is, independent compilations are maximised by
default in native mode. Moreover, we make sure that there is no
circularity in the dependencies if the programmer, by mistake,
references the current module from within it.

The macro `forge_dep` is used when deriving the dependencies of a
virtual interface from the dependencies of the implementation. This is
simply done by removing the last dependency *mod*`.cmi` on the line,
which was placed there by the macro `mk_dep`, and otherwise would
create a dependency of *mod*`.cmi` upon itself (circularity).

### Rules

The rules for updating compilation dependencies depend on the presence
of a tag file and whether the unit is a standalone implementation,
yielding four rules, but only two recipes: one calling the macro
`forge_dep` for standalone implementations and another calling
`mk_dep` otherwise.

The rules for updating compilations dependencies are only enabled when
GNU Make has not restarted, or the object files are logged, or no
dependencies have been explicitly requested (`NO_DEP` set to `"yes"`):
this in order to avoid redundancy like printing twice the same syntax
errors, and efficiency reasons.

A file `.`*mod*`.ml.odp` contains a variant of the default compilation
dependencies in `.`*mod*`.ml.dep` modified in order to maximise
opportunities for cross-module optimisations and inlining in native
mode. It consists in a postprocessing of `.`*mod*`.ml.dep` so the
result contains only `.cmx` dependencies, except for interfaces
without implementations (so-called *standalone interfaces*) and the
interface of the target itself.

A file `.`*mod*`.ml.zod` contains a version of the default compilation
dependencies in `.`*mod*`.ml.dep` modified in order to find a correct
linking order if `OBJ` is undefined (see section *Build cycle*
above). Basically, it is the same as `.`*mod*`.ml.odp`, except that
all object interfaces `.cmi` have been renamed into object
implementations `.cmx`, even if those do not exist. (They may exist in
later, and depending on the `.cmx` would then become necessary; if
they do not exist, they will be ignored anyway.)


## Generated lexers and parsers (metaprogramming)

A lexer specification *mod*`.mll` is compiled by ocamllex, which
produces an implementation *mod*`.ml`. Error messages are recorded in
the hidden file `.`*mod*`.mll.err`. A parser specification *mod*`.mly`
is translated by the macro `mk_par` with Menhir into an interface
*mod*`.mli` and an implementation *mod*`.ml`. Error messages are
stored in the hidden file `.`*mod*`.mly.err`. Note that the same
compilation unit cannot be generated by ocamllex and Menhir, that is,
having both *foo*`.mll` and *foo*`.mly` is invalid.

If an error file `.`*mod*`.mll.err` or `.`*mod*`.mly.err` is newer
than its specification *mod*`.mll` or *mod*`.mly`, the error message
it contains is reprinted, otherwise the specification is processed
after erasing previous generated lexers and parsers, together with
their associated object codes and metadata. (This ensures consistency
even if the cycle is interrupted.) If no error is detected by
ocamllex or Menhir, any error file `.err` is removed (except in
case of a warning being produced). If an error occurred, the message
is displayed and empty lexers or parsers (so-called *stubs*) are
created, together with their associated metadata `.`*mod*`.mli.ign`
and/or `.`*mod*`.ml.ign`. The reason for producing empty lexers and
parsers in case of error is that the compilation of the
implementations needed by the main module implementation (see sectuib
*Build cycle* above) must not print again those errors: empty
compilation units would then be detected and silently ignored because
they appear up to date. Moreover, metadata `.`*mod*`.mll.err.dis` and
`.`*mod*`.mly.err.dis` are produced the first time *mod*`.mll` and
*mod*`.mly` fail to produce a lexer and a parser. These are empty
files which denote that the error messages `.`*mod*`.mll.err` and
`.`*mod*`.mly.err` have been displayed. Later, the first time the
corresponding stubs are considered and ignored, these metadata are
silently erased. Afterwards, the error messages are displayed
again. In other words, `.`*mod*`.mll.err.dis` and
`.`*mod*`.mly.err.dis` are used to create a session which includes the
generation of the lexers and parsers and their subsequent compilation.

In the case of Menhir, LR(1) conflicts, if any, are stored in
*mod*`.conflicts`. Unfortunately, it is not clear how to
systematically distinguish errors from warnings, and they are lumped
together.

Furthermore, note that the list of source filenames in `.src` is
updated as soon as possible.

The parser implementation *mod*`.ml` depends on its interface
*mod*`.mli`, with no associated recipe, but the interface *mod*`.mli`
depends on the specification *mod*`.mly`. The reason is to ensure that
the macro `mk_par`, generating the parsers, is called only once for
each parser and, because the second stage of a build cycle updates
interfaces (see section *Build cycle* above), it will trigger the
parser generation. Of course, it may happen that *mod*`.ml` is
modified or removed after a successful parser generation, in which
case, *mod*`.mli` being up to date, *mod*`.ml` would not be
updated. To avoid this, the actual first step of the makefile consists
in detecting source file deletions and moving the directory as close
as possible to a consistent state, in this case, the deletion of
either the interface or the implementation yields the deletion of the
other, and so is the issue mentioned above avoided: *mod*`.ml` *and*
*mod*`.mli` are always updated simultaneously.

The makefile also handles the case of Menhir specifications
*mod*`.mly` which are not meant to generate code, but only used to
complete another parser specification. In that case, the makefile
keeps track of the modifications of these meta-dependencies
(dependencies between files that are used to generate other files). We
leverage the second expansion feature of GNU Make to achieve that
goal: we read the tag file of the main parser specification, extract
the meta-dependencies and inject them as (normal) prerequisites to the
*generated* source of the main specification. This way, any change in
those dependencies will trigger a new parser generation and
compilation of the produced code.

### Macro `mk_par`

The macro `mk_par` is called to generate a parser from a Menhir
*mod*`.mly` specification. Its first parameter is *mod*, and the
second allows the caller to add extra flags to the call of Menhir,
which enables calling Menhir with the option `--infer`. See macro
`comp_unit` below.

## Compilation

A design principle is that the recipes for compiling are the only ones
in the makefile allowed to write the files `.`*mod*`.mli.sem` and
`.`*mod*`.ml.sem`, which contain semantic errors, as opposed to syntax
errors found exclusively in `.`*mod*`.mli.syn` and `.`*mod*`.ml.syn`,
and managed exclusively by the macro `mk_dep` (dependency generator).

The section about compilation must appear *after* the inclusion of the
compilation dependencies. The variable `DEP` lists all the dependency
files *mod*`.mli.dep` and *mod*`.ml.dep` for all *mod*`.mli` and
*mod*`.ml`, respectively. The dependencies of the implementations must
be listed *before* the interfaces, so the latter are updated first
(default order of GNU Make). This ensures that the interfaces of the
parsers generated by Menhir are updated before the
implementations... which depend on the interfaces. (See above.)

### Macro `compile`

The macro `compile` is common to all kinds of compilation and never
called directly. Its purpose is to run the appropriate OCaml compiler,
producing either bytecode or native code, log any error in a file
`.`*mod*`.mli.sem` or `.`*mod*`.ml.sem`, print the message on the
terminal, and similarly for warnings in `.`*mod*`.mli.wrn` and
`.`*mod*`.ml.wrn` files. The first parameter is the name of the source
file to compile, the second is either `".cmi"`, `".cmo"` or `".cmx"`,
that is, the suffix extension of the target, and the third is the name
of the file containing warnings.

At the start of the macro, any previous warning is erased and, if
compiling an interface, any object code for the corresponding
implementation is erased. The reason is as follows. After successfully
building both the bytecode and native code binaries, if a standalone
implementation *mod*`.ml` is modified and bytecode compilation
requested, both *mod*`.cmi` and *mod*`.cmo` are updated. Afterwards,
the problem is that the recompilation to native code of a module
depending on *mod*`.cmi` would use the out-of-date *mod*`.cmx` because
the native compiler requires, like the bytecode compiler, the `.cmi`
of modules depended upon, but also the `.cmx`, if available, for
opportunistic cross-module optimisations. Therefore, we enforce a
consistent state before running the compilers by deleting all object
codes of the implementations.

Afterwards, we determine what packages managed by Findlib
(`ocamlfind`) contain dependent modules. For instance, if a
compilation unit contains the sentence

    open FilePath

we determine that `fileutils` is the package containing the module
`FilePath`, if it is managed by Findlib (which, in turn, is used by
opam]). The comma-separated list of needed packages are put in
`.`*mod*`.mli.pack` or `.`*mod>*`.ml.pack`, depending on the extension
of the source to compile, to be used later by the linker. The list of
modules it depends upon which are not found in the source directory is
extracted (from the `.ext` metadata file -- see section *Creations*
above). In other words, we gather the external dependencies, which
were unresolved locally. Then the list of managed packages is checked
for a match with the previous list: any hit is a module that is
referenced and actually managed by Findlib. The corresponding package
is found and stored in the variable `package`.

The macro `compile` is the only macro where objects are modified or
deleted: other macros handling compilation only process metadata, like
error files. In case of error, any object from a prior compilation is
deleted and *mod*`.mli` or *mod*`.ml` is marked to be skipped by
creating an empty file `.`*mod*`.mli.ign` or `.`*mod*`.ml.ign`,
respectively.

The macro `ignore` is called when the source is not compiled, either
because it was determined that it is still erroneous, or one of its
dependencies is erroneous and we want the programmer to know that it
is ignored. That last bit is achieved by touching the file
`.`*mod*`.mli.ign` or `.`*mod*`.ml.ign`, where *mod*`.mli` or
*mod*`.ml` is the source.

### Macro `comp_unit`

The macro `comp_unit` is a wrapper around the macro `compile`, whose
purpose is the compilation of interfaces and implementations which
have interfaces. In particular, it is *not* used for implementations
without interfaces (standalone implementations). See macro
`comp_stand` below.

We first check whether a syntax error occurred during the
determination of the compilation dependencies (by ocamldep) or if the
unit is empty or absent. If so, we do nothing, not even call the macro
`ignore`, because a syntax error would already have been displayed
earlier (see section *Compilation dependencies*), and an empty file
means that there is an erroneous lexer or parser specification
*mod*`.mll` or *mod*`.mly`. (Files *mod*`.mli` and *mod*`.ml` are
always generated from *mod*`.mly`, even empty.) We do not inform the
programmer that we ignore erroneous *mod*`.mli` or *mod*`.ml` when
these have been automatically generated because they are not part of
the original code base, therefore should not be overwritten manually.

The second step consists in using the macro `chk_dep` to avoid useless
compilations, by looking at the `.ign` and `.sem` files associated
with the dependencies. First, we want to know if one of them at least
has been ignored (either because it is erroneous or it depends
transitively on an erroneous unit), in which case the current unit has
to be explicitly ignored as well, that is, a message is displayed and
the unit is marked to be ignored. Second, if the current unit was
found to be erroneous during a previous run of the compiler, we want
to know whether one at least of its dependencies has been successfully
compiled *afterwards*. If so, we need to recompile the unit because
its earlier failure to compile was perhaps due to its dependency
failure to compile. Another reason to recompile the unit is when it,
or its associated tag, has been modified after it was found to be
erroneous by the previous run of a compiler. Otherwise, the error
message is simply redisplayed. Notice that we determine whether the
object code is expected to be native code or bytecode by passing
`${suffix $@}` to the macro `compile`.

### Macro `comp_stand`

The macro `comp_stand` is a wrapper around `compile` and is tailored
for the compilation of implementations without associated interfaces
(what we call *standalone implementations*). By calling the macro
`chk_dep`, it performs some checks in order to avoid useless
compilations, and informs the programmer without redundancy. (See
macro `comp_unit` above.) The macro `comp_stand` is actually called to
update *mod*`.cmi` targets, even though the prerequisite is always
*mod*`.ml`. That is because both *mod*`.cmi` and *mod*`.cmo` are
expected to be produced by the compiler from the same implementation
*mod*`.ml`. The consequence is that `comp_stand` is similar to
`comp_unit`, the main difference being that any error while compiling
the implementation will be considered an error on the virtual
interface because we want these errors to be reported as soon as
possible, that is, when interfaces are compiled (see macro
`mv_metadata and sectuib *Build cycle* above).

The macros `comp_stand` and `comp_unit` log the basenames of the
standalone implementations which failed to compile or were
ignored. This log file, named `.`*main*`.ign`, where *main* is the
basename of the executable, is later used by the macro `prelink` (see
section *Linking* below) to avoid linking these modules.

Yet another difference is related to the need to manage builds
alternating from bytecode to native code, and vice versa, without
useless recompilations or linkings. The object codes for the
interfaces are the same if obtained by compilation to bytecode or to
native code and this proves to be an issue with standalone
implementations. To avoid each kind of build stepping on each other's
feet, namely the common interface object, we proceed as follows. We
set the variable `NATIVE` to a non-empty value if the GNU Make command
goal (there should be only one) contains a target which requires
compilation to native code. The macro `comp_stand` determines, by
means of a recursive call to the makefile with option `-q`
(`--question`), whether the alternate binary is up to date, e.g., if
the current build is bytecode, we check whether the native code
version is up to date. If it is, the object code of the virtual
interface is saved before compilation, and restored afterwards, in
order to preserve its time stamp, hence the status of the alternate
binary. The catch is that, when running GNU Make with the option `-j`,
that is, in parallel build mode, this procedure is flawed because of a
race condition between the Make process that checks the time stamps of
the compiled interface and the process that runs the recipe just
described: the first may find the time stamp of the newly created
`.cmi`, instead of the one after restoration, which is also the time
stamp of the compiled interface before compilation. GNU Make would
then proceed to rebuild objects which are already up-to-date. The
theoretical solution would be to have the call to the macro `compile`
and the subsequent restoration (the shell command `mv`) be in an
critical section. Unfortunately, this is not possible, because the
first process does not run a copy of the recipe, but is instead a
process run by make to determine whether prerequisites are up to date
or not: no solution on a lock file would work. Our approach is to run
the Makefile in sequential mode (no `-j` option), as the setting

    MAKEFLAGS =-Rrs

at the beginning reveals. If we determine that the last build was made
for the same target code than the present one, for instance, we last
built bytecode and we request bytecode again, then we build in
parallel the new targets, by setting the `-j -Oline` flag to the
recursive sub-make. See below. In other words, when alternating from
bytecode to native, or vice-versa, the build is always performed
sequentially, hence there the race condition described above cannot
happen. Otherwise, it is performed in parallel, but the race condition
cannot occur because object interfaces do not need to be saved and
restored. See sectuib *Linking* below.

### Rules

First, note that the rules for compiling units do not depend on tag
files. Instead, the object implementations (`.cmo`) to update depend
on compilation dependency files (`.dep`), if they have an interface,
otherwise they depend on the object interface (`.cmi`). Transitively,
this entails that they do depend on source and tag files, by means of
the rules updating the dependencies.

The compilation of interfaces is already understood by reading above
the details about the macros `comp_unit` and `comp_stand`. The curious
rule `%.cmi: ;` is useful after a successful build followed by the
deletion of a (needed) interface without implementation: this rule
will allow us to ignore the problem and trigger the recompilation of a
module which still depends on it, yielding an informative error
message instead of the cryptic

    *** No rule to make target `XXX.cmi', needed by `YYY.cmi'.  Stop.

Note also the similar rules `%.cmo: ;` and `%.cmx: ;`, which are useful
after a successful build followed by the deletion of an
implementation: in that case, the linker will report an error about a
missing implementation object.

The standalone implementations require special care, as usual. Note
that the prerequisite is an object interface, contrary to the modules
with interfaces, which depend on the compilation dependencies of its
source. The reason for that difference is that we have to tackle two
cases: either the interface has already been compiled by `comp_stand`
in the rule `%.cmi: .%.ml.dep`, or we are switching from bytecode mode
to native, or vice-versa, in which case, a change in the object
interface (due to the previous alternate mode) must trigger the
compilation of the implementation (in the new mode). Also, there is
the unlikely case that the `.cmo` has been manually erased, in which
case it would be regenerated here (but only if it must not be
ignored).

Note: The existence of warnings is reported the first time they are
produced by a compiler, but they need to be explicitly requested after
that by `make warn`.


## Linking

By default, linking dependencies are a topological sort of the
compilation order, derived from the output of ocamldep. This is
sound only if no interfering side-effects occur between top-level
values at link-time. Otherwise, the semantics of the program would
depend on this order and the programmer must specify the objects to
link and their order in a tag file (whose name is `.`*mod*`.tag`,
containing a line starting with

    objects:

and followed by the list of the basenames of the objects in the
linking order). The variable `LOG_OBJ` is set to `"yes"` in a
recursive call if, and only if, the linking dependencies are
determined by the makefile and logged in the hidden file
`.`*main*`.lnk`, where *main* is the basename of the executable being
built. Consider now the rules for making the executable (see "Linking
bytecode" and "Linking native code" in Makefile).

### Case when `OBJ` is undefined

If `OBJ` is undefined, a recursive call is performed, updating
*main*`.cmo` or *main*`.cmx`, where *main*`.ml`, *main*`.mll` or
*main*`.mly` is the main module or the specification of the main
module. Note that the assignment

    MARK_IGN:=yes

means that we want erroneous interfaces and implementations to be
recorded in the hidden file `.`*main*`.ign` by the macros `comp_unit`
and `comp_stand`, because we do not want to require the corresponding
objects for linking later on (yielding again the same error). By the
way, this is why `.`*main*`.ign` is removed just before the recursive
call (the macros only add file names to `.`*main*`.ign`). Note how we
request a parallel build with

    MAKEFLAGS=-Rrsj -Oline

if, and only if, we have determined that we previously built the same
kind of target (for example, bytecode then, bytecode now). See macro
`comp_stand` above. In passing, we see that we record the kind of
build as an empty, hidden file: either `.`*main*`.byte` or
`.`*main*`.nat`.

### Macro `prelink`

After returning from the recursive call, the macro `prelink` is
executed. First, the linking order is updated by a recursive call
whose target is the hidden file `.`*main*`.lnk`. That file depends on
special compilation dependencies `.zod` (see above *Compilation
dependencies*). If all those dependencies have been successfully
generated, the macro `update_links` is called.

### Macro `update_links`

The macro `update_links` calls recursively GNU Make and forces it to
assume that all the implementations are new. As a side-effect of
setting

    LOG_OBJ:=yes

the basenames of the objects required to build `${BIN}.cmx` are logged
into the file `.`*main*`.lnk`. The rules for creating this file are
twofold: a pair of rules for interfaces and one rule for object
implementations `%.cmx`. The former pair consists of a rule for making
the object of interfaces without implementations and the second rule
is for making the other objects. In the first case, there is no recipe
because only implementations matter to the linker; in the second case,
the prerequisite is simply the corresponding `%.cmx`, without recipe
either. The rule for making [%.cmx] does all the logging.

Note that, in the recursive call in the macro `update_links`, the
redirection of the standard error to the file `.`*main*`.circ`
collects the dependency circularities when logging the `.cmx` into
`.`*main*`.lnk`, because we have the rule `%.cmi: %.cmx` and we made
sure that, from the output of ocamldep, all `%.cmx` depend on their
`%.cmi`. If all compilations succeeded, these circularities are not a
theoretical problem because they are the result of *a*`.ml` depending
on *b*`.mli` and *b*`.ml` depending on *a*`.mli`, with one dependency
at least being upon a type. Nevertheless, we do not try to sort the
objects and instead report the problem, as this an unusual situation
which may actually reveal a design flaw.

Once `.`*main*`.ign` and `.`*main*`.lnk` have been updated, the
objects in the former file are removed from the latter, because they
will not be available for linking. If the object of the main module is
available for linking, a recursive call to GNU Make is performed with
the executable as a target and the sorted object codes as the value of
`OBJ`. Naturally, this brings us to the case when `OBJ` is defined.

Case when `OBJ` is defined (macro `link`)

Linking is forced when a critical deletion has been made (a
non-critical deletion is the deletion of an interface which has an
associated implementation. See `.del` and section *Sections*), even if
the dependent objects in `${OBJ}` are up to-date. The macro in charge
of calling the linker is `link`. The design principle is that linking
is attempted if, and only if, no compilation error occurred when
updating the objects to link, and at least one of these objects is
newer than the last error message from the linker, if any. (This is
similar to the way we follow in macro `chk_dep`. See macro `comp_unit`
above.) Only objects actually built will be passed to the linker, and
the presence amongst them of the object of the main module is
necessary. Any linking error is recorded in the hidden file `.$@.err`
and any stub is removed as in the previous case, so they will need
updating at the next build cycle. Note that the list of dependent
packages managed by Findlib (`ocamlfind`) is collated and passed to
the linker, as it was to the compiler.


## Lessons learnt

The keys were

  * to understand all the features of the two OCaml compilers, in
    particular the silent and opportunistic optimisations, and how the
    bytecode and native code compilers interact;

  * do not fear recursive calls to GNU make, in particular, starting
    in the source directory, and then restarting after jumping to the
    build directory and setting VPATH to the source directory: this
    avoids differences in paths when Make matches targets and
    prerequisites;

  * to know what GNU Make really does and augment its graph traversals
    with metadata whose invariants are explicit and checked with some
    measure of redundancy;

  * to take advantage of some options of ocamldep, but also undo
    some of its work so the compilation dependencies become very
    uniform, allowing the Makefile to process them more easily;

  * to use a minimal set of GNU Make+dash features and shell commands;
    detect and avoid bashisms;

  * to mind case-insensitive and case-preserving operating systems;

  * to associate tag files to each source file and executable, to
    tailor each processing (code generation, compilation, linking);

  * to aim at printing only independent errors and minimising
    recompilations;

  * to never force a clean slate on the user (the dreaded `make
    clean`);

  * depending on the version of GNU Make available, to enable multiple
    updates in parallel without destroying the display;

  * to propose a development method to minimise recompilations;

  * to document the design and write a manual.
