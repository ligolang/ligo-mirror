# GNU Makefile (>= 4.0) for building OCaml applications
# (c) 2012-2020 Christian Rinderknecht
# rinderknecht@free.fr

# ====================================================================
# TO DO

# Bug: From scratch, [make] then error because Version.ml is missing,
# then [make sync], then [make] fails (it should not).

# Add a catch-all rule
# Inform about useless modules (neither compiled nor linked)

# ====================================================================
# General Settings (GNU Make 4.1 recommended)

# Checking version of GNU Make

AT_LEAST := 4.0
OK := ${filter ${AT_LEAST}, \
               ${firstword ${sort ${MAKE_VERSION} ${AT_LEAST}}}}

ifeq (,${OK})
${error Requires GNU Make ${AT_LEAST} or higher}
endif

# Name of the current makefile

THIS := ${notdir ${lastword ${MAKEFILE_LIST}}}

# Debugging information

ifeq (,${MAKECMDGOALS})
  ${if ${DEBUG},${info No command goals.}}
else ifeq (${words ${MAKECMDGOALS}},1)
  ${if ${DEBUG},${info Command goal: ${MAKECMDGOALS}}}
endif

${if ${DEBUG}, \
     ${info MAKELEVEL-MAKE_RESTARTS=${MAKELEVEL}-${MAKE_RESTARTS}}}

# Verbosity and debugging modes

ifeq (yes,${DEBUG})
  override VERB := yes
endif

export VERB DEBUG TRACE

# In-place GNU Sed

export I :=${if ${DEBUG},-i.old,-i}

# Setting the flags of GNU Make (no built-in rules, silent)

MAKEFLAGS =-Rrs

# Setting some variables of GNU Make

.DELETE_ON_ERROR:
.ONESHELL:        # One call to the shell per recipe
.RECIPEPREFIX = > # Use '>' instead of TAB for recipes
.SUFFIXES:        # Removing (almost) all built-in rules
.SECONDEXPANSION:

# GNU Make should not try to update any makefile, nor tag files

Makefile GNUmakefile makefile Makefile.cfg: ;
.%.tag: ;

# A prerequisite forcing the update of its target

.PHONY: FORCE
FORCE: ;

# By default, do not infer linking dependencies (that is, do not log
# objects).

export LOG_OBJ := no

# By default, not a session

export SESSION :=

# Directory for object files, executables and metadata

ifndef OBJDIR
export OBJDIR := _${shell arch}
endif

# Directory for tests

ifndef TSTDIR
export TSTDIR := ${OBJDIR}/tests
endif

# If present, [ocamlfind] will be used to drive the compilers and any
# setting [OCAMLC := ocamlc.opt] or [OCAMLOPT := ocamlopt.opt] will
# then be overriden because [ocamlfind] takes care of calling these
# optimised compilers implicitly. Otherwise, the native-code versions
# of [ocamldep], [ocamllex], [ocamlc] and [ocamlopt] are selected,
# that is, [ocamldep.opt], [ocamllex.opt], [ocamlc.opt] and
# [ocamlopt.opt].

ifndef OCAMLFIND
export OCAMLFIND := ${shell which ocamlfind}
endif

ifdef OCAMLFIND
  override OCAMLDEP ?= ocamldep
  override OCAMLC   ?= ocamlc
  override OCAMLOPT ?= ocamlopt
  LIB_PATH := ${shell ${OCAMLFIND} printconf path}
else
OCAMLDEP ?= ${if ${shell which ocamldep.opt 2>/dev/null},\
                  ocamldep.opt,ocamldep}
OCAMLC   ?= ${if ${shell which ocamlc.opt 2>/dev/null},\
                 ocamlc.opt,ocamlc}
OCAMLOPT ?= ${if ${shell which ocamlopt.opt 2>/dev/null},\
                 ocamlopt.opt,ocamlopt}
LIB_PATH := ${${OCAMLC} -where}
endif

OCAMLLEX ?= ${if ${shell which ocamllex.opt 2>/dev/null},\
                 ocamllex.opt,ocamllex}

# Printing number of lines

define line_count
case "$1" in \
  0) printf " (empty).";; \
  1) printf " (1 line).";; \
  *) printf " ($1 lines).";; \
esac
endef

# Printing failures or emphasising in red

define failed
printf "\033[31mFAILED$1\033[0m\n"
endef

define display
printf "\033[31m"; cat $1; printf "\033[0m"
touch $1.dis
endef

define emphasise
printf "\033[31m$1\033[0m\n"
endef

# Macros identifying metadata by their source

from_mli = .$1.mli.syn .$1.mli.sem .$1.mli.wrn \
           .$1.mli.ign .$1.mli.dep $1.cmi

from_ml = .$1.ml.syn .$1.ml.sem .$1.ml.wrn .$1.ml.ign \
          .$1.ml.dep .$1.ml.odp .$1.ml.zod $1.cmo $1.cmx

# Cleaning targets

CLEANING := clean mostlyclean clean_obj clean_bin clean_stubs \
            clean_dep clean_displays

.PHONY: ${CLEANING}

# Extracting compilation dependencies

ifeq (0,${MAKELEVEL})
override DFLAGS += -modules -one-line
endif

# Generating parsers with Menhir (in the source directory)
#
# $1: basename of the Menhir specification
# $2: extra options for Menhir (other than those in the tag file)

define mk_par
${if ${DEBUG},echo "Entering mk_par ($1.mly)."}
mly=$1.mly
conflicts=$1.conflicts
tag=.$$mly.tag
mli=$1.mli
ml=$1.ml
obj="${OBJDIR}/.$1.cmi ${OBJDIR}/.$1.cmo ${OBJDIR}/.$1.cmx"
out=${OBJDIR}/.$$mly.out
err=${OBJDIR}/.$$mly.err
ign=${OBJDIR}/.$$mly.ign
wrn=${OBJDIR}/.$$mly.wrn
mli_ign=${OBJDIR}/.$$mli.ign
ml_ign=${OBJDIR}/.$$ml.ign
src=${OBJDIR}/.src
del="$$mli $$ml $$err $$wrn \
     $1.output $1.automaton $1.conflicts \
     ${OBJDIR}/.$$mli.syn ${OBJDIR}/.$$mli.sem ${OBJDIR}/.$$mli.wrn \
     ${OBJDIR}/.$$ml.syn ${OBJDIR}/.$$ml.sem ${OBJDIR}/.$$ml.wrn \
     $$mli_ign $$ml_ign"

if test -e $$ign; then
  up=no
  if test -n "$3"; then
    for dep in $3; do
      if test -n "$$(find -L $$dep -newer $$ign 2>/dev/null)"; then
        up=yes; break; fi
    done
  elif test -n "$$(find -L $$mly -newer $$ign 2>/dev/null)"; then
    up=yes
  elif test -e $$tag; then
    if test -n "$$(find -L $$tag -newer $$ign 2>/dev/null)"; then
      up=yes; fi; fi
else up=yes; fi

if test "$$up" = "no"; then
  if test -e $$err.dis; then
    echo "Ignoring $$mly."
  elif test -e $$err; then
    ${call display,$$err}
    if test "${DEBUG}" = "yes"; then
      echo "Ignoring $$mly."; fi; fi
  touch $$mli $$mli_ign $$ml $$ml_ign $$ign
  rm -f $$obj
else
  if test "$4" != "infer"; then rm -f $$del; fi
  lines="$$(wc -l $$mly | sed -e 's/ *\([0-9]\+\) .*/\1/g')"
  printf "Making $$ml(i) from $$mly"
  flags='$2'" $$(echo $$(cat $$tag 2>/dev/null))"
  ${call line_count,$$lines}
  printf ".. "

  if test "${TRACE}" = "yes"; then
    echo "${notdir ${MENHIR}} $$flags $$mly" \
  | tr -s ' ' >> ${OBJDIR}/build.sh; fi

  eval ${strip ${MENHIR} $$flags $$mly > $$out 2>&1}

  if test "$$?" = "0"; then
    printf "done"
    echo $$mli >> $$src
    echo $$ml  >> $$src
    sort -u -o $$src $$src
    if test -s "$$out"; then
      printf ":\n"
      warning=$$(grep '^Warning' $$out)
      if test -n "$$warning"; then
        echo "$$warning"; fi
      pager=$$(grep -w pager $$out)
      if test -n "$$pager"; then
        echo "$$pager"
        sed ${I} '/\<pager\>/d' $$out; fi

      built=$$(grep '^Built' $$out)
      if test -n "$$built"; then
        echo "$$built"
        sed ${I} '/^Built /d' $$out; fi

      note=$$(grep '^Note' $$out)
      if test -n "$$note"; then
        echo "$$note"; fi
      conflicts=$$(grep 'shift/reduce' $$out)
      if test -n "$$conflicts"; then
        printf "\033[31m$$conflicts\033[0m\n"; fi
      extra=$$(grep '^Extra' $$out)
      if test -n "$$extra"; then
        echo "$$extra"
        sed ${I} '/^Extra /d' $$out; fi
     priority=$$(grep '^Priority' $$out)
     if test -n "$$priority"; then
       echo "$$priority"
       sed ${I} '/^Priority /d' $$out; fi
      if test -s "$$out"; then mv -f $$out $$wrn; fi
    else
      printf ".\n"
      rm -f $$out;
      if test -e $$conflicts; then rm -f $$conflicts; fi
    fi
  else
    ${call failed,:}
    rm -f $$obj
    ${call display,$$out}
    if test "$4" = "infer"; then
      mv $$out ${OBJDIR}/.$$ml.sem
      touch ${OBJDIR}/.$$ml.ign ${OBJDIR}/.$$ml.sem.dis
    else
      mv $$out $$err
      rm -f $$mli $$ml
      touch $$mli $$mli_ign $$ml $$ml_ign $$ign $$err.dis
      echo "Ignoring $$mly."; fi; fi; fi
endef

# Detecting whether we are in the build or source directory
#
ifeq (${notdir ${OBJDIR}},${notdir ${CURDIR}})

# ====================================================================
# We are in the build directory ${OBJDIR}

# Where are the source files?

ifndef SRCDIR
${error > Please set the SRCDIR variable.}
else

# We will seek prerequisites in the source directory, where we come
# from (see end of this makefile).

VPATH = ${SRCDIR}

# Macros identifying some metadata

all_mli  = ${SRCDIR}/$1.mli ${call from_mli,$1}
all_ml   = ${SRCDIR}/$1.ml ${call from_ml,$1}
from_mll = .$1.mll.err ${call all_ml,$1}
from_mly = .$1.mly.err ${call all_ml,$1} ${call all_mli,$1} \
           ${SRCDIR}/$1.output ${SRCDIR}/$1.automaton \
           ${SRCDIR}/$1.conflicts

# Cleaning the slate

mostlyclean:
> rm -fr *.cmi *.cmo *.cmx *.cma *.cmxa *.o \
         .src .src.new .ext .del .pack .pack.new \
         .*.lnk .*.circ .*.byte .*.opt \
         .*.dep .*.odp .*.zod .*.ign .*.old \
         .*.syn .*.sem .*.wrn .*.err .*.pack \
         .*.rem .*.out .*.err.dis .*.sem.dis \
         .*.lib .*.incl build.sh

clean_bin:
> rm -fr *.byte *.opt .*.byte.err .*.opt.err .*.byte.wrn .*.opt.wrn

clean_obj: clean_bin
> printf "Cleaning objects... "
> rm -fr *.cmi *.cmo *.cmx *.cma *.cmxa *.o \
         .*.sem .*.mli.wrn .*.ml.wrn .*.c.err
> echo "done."

clean_dep:
> printf "Cleaning compilation dependencies... "
> rm -fr .*.dep .*.pack .pack .pack.new .*.lib .*.incl
> echo "done."

# Objects to link

ifneq (0,${MAKELEVEL})
  ifneq (,${BIN})
OBJ := ${shell sed -n "s/^objects: \(.*\)$$/\1/p" \
                      ${SRCDIR}/.${BIN}.tag 2>/dev/null}
  endif
endif

# Including the configuration located in the source directory

sinclude ${SRCDIR}/Makefile.cfg

# The parser generator is Menhir

MENHIR ?= menhir

# ====================================================================
# Restoring consistency after deletions

# Removing metadata of deleted sources

meta_no_dep = .$1.ign .$1.pack .$1.lib .$1.incl \
              .$1.syn .$1.sem .$1.wrn .$1.err \
              .$1.err.dis .$1.sem.dis .$1.syn.dis

metadata = .$1.dep .$1.odp .$1.zod ${call meta_no_dep,$1}

.del:
> touch .del

# Synchronising with the new build state

define sync
if test "${VERB}" = "yes"; then
  printf "Synchronising with the new build state... "; fi
if test "${DEBUG}" = "yes"; then echo; fi

# Preparing the build script (may remain empty if no TRACE=yes)

printf "#!/bin/sh\nset -x\n" > build.sh

# Creating the OCaml module containing the current commit number as a
# version number

hash=$$(git describe --always --dirty)
printf "let version = \"%s\"\n" $$hash > ${SRCDIR}/Version2.ml
if test ! -e ${SRCDIR}/Version.ml \
|| (! diff -q ${SRCDIR}/Version2.ml ${SRCDIR}/Version.ml >/dev/null); \
then cat ${SRCDIR}/Version2.ml > ${SRCDIR}/Version.ml; \
fi; \
rm -f ${SRCDIR}/Version2.ml


# Recording sources and tags

ls ${SRCDIR}/*.ml ${SRCDIR}/*.mli ${SRCDIR}/*.mll ${SRCDIR}/*.mly \
   ${SRCDIR}/.*.tag 2>/dev/null \
| xargs -n1 basename | sort -u > .src.new

# Recording installed packages

if test -n "${OCAMLFIND}"; then
  ${OCAMLFIND} list \
| sed -n "s/^\([^ \.-]*\).*/\u\1/p" \
| sort -u > .pack.new
fi

# Collecting objects from standalone modules

all_cmi=$$(ls *.cmi 2>/dev/null | xargs -n1 -I/ basename / .cmi)
for a_cmi in $$all_cmi; do
  if echo ${IMPL_ONLY} | grep -w $$a_cmi > /dev/null 2>&1; then
    cmi="$$cmi $$a_cmi"; fi
done

all_cmo=$$(ls *.cmo 2>/dev/null | xargs -n1 -I/ basename / .cmo)
for a_cmo in $$all_cmo; do
  if echo ${IMPL_ONLY} | grep -w $$a_cmo > /dev/null 2>&1; then
    cmo="$$cmo $$a_cmo"; fi
done

all_cmx=$$(ls *.cmx 2>/dev/null | xargs -n1 -I/ basename / .cmx)
for a_cmx in $$all_cmx; do
  if echo ${IMPL_ONLY} | grep -w $$a_cmx > /dev/null 2>&1; then
    cmx="$$cmx $$a_cmx"; fi
done

# Collecting orphan objects from standalone modules

for a_cmi in $$cmi; do
  if ! (echo "$$cmo $$cmx" | grep -w $$a_cmi > /dev/null 2>&1);
  then orphan_cmi="$$orphan_cmi $$a_cmi"; fi
done
for a_cmo in $$cmo; do
  if ! (echo $$cmi | grep -w $$a_cmo > /dev/null 2>&1); then
    orphan_cmo="$$orphan_cmo $$a_cmo"; fi
done
for a_cmx in $$cmx; do
  if ! (echo $$cmi | grep -w $$a_cmx > /dev/null 2>&1); then
    orphan_cmx="$$orphan_cmx $$a_cmx"; fi
done

# Deleting object files from standalone modules

for a_cmi in $$orphan_cmi; do
  if test "${DEBUG}" = "yes"; then
    printf "Deleting $$a_cmi.cmi... "; fi
  rm -f $$a_cmi.cmi
  if test "${DEBUG}" = "yes"; then echo "done."; fi
done

for a_cmo in $$orphan_cmo; do
  if test "${DEBUG}" = "yes"; then
    printf "Deleting $$a_cmo.cmo... "; fi
  rm -f $$a_cmo.cmo
  if test "${DEBUG}" = "yes"; then echo "done."; fi
done

for a_cmx in $$orphan_cmx; do
  if test "${DEBUG}" = "yes"; then
    printf "Deleting $$a_cmx.cmx... "; fi
  rm -f $$a_cmx.cmx
  if test "${DEBUG}" = "yes"; then echo "done."; fi
done

# Determining newly created modules and enabling the compilation of
# the units that depend on them

if test -e .src; then
  new_mods=$$(comm -1 -3 .src .src.new \
              | sed -n "s/\(.*\)\.ml\(\|i\|l\|y\)/\u\1/p" \
              | sort -u); fi

for mod in $$new_mods; do
  if test -z "$$dep_to_del"; then
    dep_to_del="'/^$$mod:/d'"
  else dep_to_del="$$dep_to_del; '/^$$mod:/d'"; fi
  lmod=$$(echo $$mod | sed "s/\<./\l&/g")
  if test -e ${SRCDIR}/$$mod.ml -o -e ${SRCDIR}/$$mod.mli; then
    module="$$mod";
  else module="$$lmod"; fi
  files=$$(sed -n "s/$$mod: \(.*\)/\\1/p" .ext 2>/dev/null | sort -u)
  for file in $$files; do
    sed ${I} "s/:/: $$module.cmi/" .$$file.dep 2>/dev/null
    sed ${I} "s/:/: $$module.cmx/" .$$file.zod 2>/dev/null
    if test -e ${SRCDIR}/$$module.ml; then
      sed ${I} "s/:/: $$module.cmx/" .$$file.odp 2>/dev/null
    else sed ${I} "s/:/: $$module.cmi/" .$$file.odp 2>/dev/null; fi
  done
done

if test -n "$$dep_to_del"; then
  sed ${I} $$dep_to_del .ext 2>/dev/null; fi

# Determining installation of packages and enabling recompilation of
# units that depend on them

if test -e .pack -a -e .pack.new; then
  new_pack=$$(comm -1 -3 .pack .pack.new)
  if test -n "${DEBUG}" -a -n "$$new_pack"; then
    echo "Newly installed packages: "$$new_pack; fi

  for mod in $$new_pack; do
    files=$$(sed -n "s/$$mod: \(.*\)/\\1/p" .ext 2>/dev/null | sort -u)
    for file in $$files; do
      mv -f .$$file.sem .$$file.sem.old
      rm -f .$$file.ign
      echo $$mod | sed "s/\<./\l&/g" >> .$$file.pack
      sort -u -o .$$file.pack .$$file.pack
    done
  done
fi

# Updating the list of packages

mv -f .pack.new .pack

# Deletions

deleted=$$(if test -e .src; then comm -2 -3 .src .src.new; fi)
if test "${DEBUG}" = "yes" -a -n "$$deleted"; then
  echo "Deleted files: "$$deleted; fi

# Removing useless or inconsistent metadata after deletions

del1=
for del in $$deleted; do
  del1="$$del $$del1"
  ext=$$(echo $$del | sed -n "s/.*\.\(.*\)/\1/p")
  if test $$ext = ml -a ! -e ${SRCDIR}/$${del}i; then
    del1="$${del}i $$del1"; fi
done

to_clean=$$(echo $$del1 | tr ' ' '\n' | sort -u | sed '/^\s*$$/d')

lnk="$$(ls .*.lnk 2>/dev/null)"
for del in $$to_clean; do
  ext=$$(echo $$del | sed -n "s/.*\.\(.*\)/\1/p")
  base=$$(basename $$del .$$ext)
  case "$$ext" in
    mli) if test "${VERB}" = "yes"; then
           printf "Deleting metadata of $$del... "; fi
         if test -e ${SRCDIR}/$$base.mly; then
           rm -f ${call meta_no_dep,$$del}
         else rm -f ${call metadata,$$del}; fi;;
     ml) if test "${VERB}" = "yes"; then
           printf "Deleting metadata of $$del... "; fi
         if test -e ${SRCDIR}/$$base.mly; then
           rm -f ${call meta_no_dep,$$del}
         else rm -f ${call metadata,$$del}; fi;;
    mll) if test "${VERB}" = "yes"; then
           printf "Deleting metadata and code generated from $$del... "
         fi
         rm -f .$$del.err .$$del.err.dis .$$del.ign \
               ${SRCDIR}/$$base.ml ${call metadata,$$base.ml}
         if test ! -e ${SRCDIR}/$$base.mli; then
           rm -f ${call metadata,$$base.mli}; fi;;
    mly) if test "${VERB}" = "yes"; then
           printf "Deleting metadata and code generated from $$del... "
         fi
         rm -f .$$del.err .$$del.err.dis \
               ${SRCDIR}/$$base.output ${SRCDIR}/$$base.automaton \
               ${SRCDIR}/$$base.conflicts \
               ${SRCDIR}/$$base.ml ${SRCDIR}/$$base.mli \
               ${call metadata,$$base.ml} ${call metadata,$$base.mli};;
  esac
  sed ${I} "/^$$base/d" $$lnk 2>/dev/null
  if test "${VERB}" = "yes"; then printf "done.\n"; fi
done

# Updating the list of source files

mv -f .src.new .src

# Restoring consistency after source deletions

del_mll=$$(echo $$(echo $$deleted | tr ' ' '\n' \
                   | sed -n "s/\b\(.*\)\.mll\b/\1/p"))
del_mly=$$(echo $$(echo $$deleted | tr ' ' '\n' \
                   | sed -n "s/\b\(.*\)\.mly\b/\1/p"))
del_mli=$$(echo $$(echo $$deleted | tr ' ' '\n' \
                   | sed -n "s/\b\(.*\)\.mli\b/\1/p"))
del_ml=$$(echo $$(echo $$deleted | tr ' ' '\n' \
                  | sed -n "s/\b\(.*\)\.ml\b/\1/p"))

add_del_mli=
for mli in $$del_mli; do
  if ! (echo $$del_mly | grep -w $$mli > /dev/null 2>&1); then
    add_del_mli="$$add_del_mli $$mli"; fi
done
del_mli="$$del_mli $$add_del_mli"

add_del_ml=
for ml in $$del_ml; do
  if ! (echo $$del_mly $$del_mll | grep -w $$ml > /dev/null 2>&1); then
    add_del_ml="$$add_del_ml $$ml"; fi
done
del_ml="$$del_ml $$add_del_ml"

for mli in $$del_mli; do
  if echo ${YMOD} | grep -w $$mli > /dev/null 2>&1; then
    del_ymli="$$del_ymli $$mli"; fi
done
del_ymli=$$(echo $$del_ymli | xargs -n1 | sort -u | xargs)

for ml in $$del_ml; do
  if echo ${YMOD} | grep -w $$ml > /dev/null 2>&1; then
    del_yml="$$del_yml $$ml"; fi
done

add_del_yml=
for ml in $$del_yml; do
  if ! (echo $$del_ymli | grep -w $$ml > /dev/null 2>&1); then
    add_del_yml="$$add_del_yml $$ml"; fi
done
del_yml="$$del_yml $$add_del_yml"
del_yml=$$(echo $$del_yml | xargs -n1 | sort -u | xargs)

add_del_mli=
for mli in $$del_mli; do
  if ! (echo $$del_ymli | grep -w $$mli > /dev/null 2>&1); then
    add_del_mli="$$add_del_mli $$mli"; fi
done
del_mli="$$del_mli $$add_del_mli"
del_mli=$$(echo $$del_mli | xargs -n1 | sort -u | xargs)

add_del_ml=
for ml in $$del_ml; do
  if ! (echo $$del_yml | grep -w $$ml > /dev/null 2>&1); then
    add_del_ml="$$add_del_ml $$ml"; fi
done
del_ml="$$del_ml $$add_del_ml"
del_ml=$$(echo $$del_ml | xargs -n1 | sort -u | xargs)

cdel=

if test -n "$$del_mll"; then cdel=$$del_mll; fi
if test -n "$$del_mly"; then
  if test -z "$$cdel"; then cdel=$$del_mly;
  else cdel="$$cdel $$del_mly"; fi; fi
if test -n "$$del_ml"; then
  if test -z "$$cdel"; then cdel=$$del_ml;
  else cdel="$$cdel $$del_ml"; fi; fi

for mli in $$del_mli; do
  if ! (echo $$ml | grep -w $$mli > /dev/null 2>&1); then
    if test -z "$$cdel"; then cdel=$$mli;
    else cdel="$$cdel $$mli"; fi; fi
done

if test -n "$$cdel"; then echo $$cdel | cat > .del; fi

new_del_mli=
for mli in $$del_mli; do
  if ! (echo $$del_ymli | grep -w $$mli > /dev/null 2>&1); then
    new_del_mli="$$new_del_mli $$mli"; fi
done
del_mli=$$new_del_mli

new_del_ml=
for ml in $$del_ml; do
  if ! (echo $$del_yml | grep -w $$ml > /dev/null 2>&1); then
    new_del_ml="$$new_del_ml $$ml"; fi
done
del_ml=$$new_del_ml

for mli in $$del_mli; do rm -f ${call from_mli,$$mli}; done
for ml in $$del_ml; do rm -f ${call from_ml,$$ml}; done

for mll in $$del_mll; do
  rm -f ${call from_mll,$$mll}
  sed ${I} "/^$$mll\.ml$$/d" .src
done

for mly in $$del_mly; do
  rm -f ${call from_mly,$$mly}
  sed ${I} -e "/^$$mly\.mli$$/d" -e "/^$$mly\.ml$$/d" .src
done

for mli in $$del_ymli; do
  rm -f $$mli.cmi ${call meta_no_dep,$$mli}
  sed ${I} "/^$$mli\.ml$$/d" .src
done

for ml in $$del_yml; do
  rm -f $$ml.cmo $$ml.cmx ${call meta_no_dep,$$ml}
  sed ${I} "/^$$ml\.mli$$/d" .src
done

# Finding deleted tags

del_tags=$$(echo $$(echo $$deleted | tr ' ' '\n' \
                    | sed -n "s/\b\.\(.*\)\.tag\b/\1/p"))

# Sorting deleted tags by extension

del_mli_tags=
for tag in $$del_tags; do
  if echo ${MLI} | grep -w $$tag > /dev/null 2>&1; then
    del_mli_tags="$$del_mli_tags $$(basename $$tag .mli)"; fi
done

del_ml_tags=
for tag in $$del_tags; do
  if echo ${ML} | grep -w $$tag > /dev/null 2>&1; then
    del_ml_tags="$$del_ml_tags $$(basename $$tag .ml)"; fi
done

del_mll_tags=
for tag in $$del_tags; do
  if echo ${MLL} | grep -w $$tag > /dev/null 2>&1; then
    del_mll_tags="$$del_mll_tags $$(basename $$tag .mll)"; fi
done

del_mly_tags=
for tag in $$del_tags; do
  if echo ${MLY} | grep -w $$tag > /dev/null 2>&1; then
    del_mly_tags="$$del_mly_tags $$(basename $$tag .mly)"; fi
done

# Restoring consistency after tag deletions

for mli in $$del_mli_tags; do
  rm -f ${call from_mli,$$mli}
done

for ml in $$del_ml_tags; do
  rm -f ${call from_ml,$$ml}
done

for mll in $$del_mll_tags; do
  rm -f .$$mll.mll.err ${call all_ml,$$mll}
done

for mly in $$del_mly_tags; do
  rm -f .$$mly.mly.err ${call all_ml,$$mly} ${call all_mli,$$mly}
done

if test "${DEBUG}" != "yes" -a "${VERB}" = "yes"; then
  echo "done."; fi
endef

.PHONY: sync
sync:
> ${call sync}

# ====================================================================
# Compilation and linking dependencies

# We add the virtual interfaces of the standalone modules, i.e.,
# modules without interfaces. WARNING: Order is relevant in right-hand
# side of MLI_DEP.

MLI_DEP := ${INTF:%=.%.mli.dep} ${IMPL_ONLY:%=.%.mli.dep}
DEP     := ${IMPL:%=.%.ml.dep} ${MLI_DEP}
ODP     := ${IMPL:%=.%.ml.odp} ${MLI_DEP}
ZOD     := ${IMPL:%=.%.ml.zod} ${MLI_DEP}

ifneq (sync,${MAKECMDGOALS})
  ifneq (,${BUILD})
    ifeq (yes,${LOG_OBJ})
      ${if ${DEBUG},${info Including .zod dependencies.}}
      sinclude ${ZOD}
    else
      ifneq (,${filter opt %.opt %.cmx, ${MAKECMDGOALS}})
        ${if ${DEBUG},${info Including .odp dependencies.}}
        sinclude ${ODP}
      else
        ifeq (,${filter .%.dep dep infer_dep,${MAKECMDGOALS}})
          ${if ${DEBUG},${info Including .dep dependencies.}}
          sinclude ${DEP}
        endif
      endif
    endif
  endif
endif

.PHONY: dep odp zod

dep: ${DEP}
odp: ${ODP}
zod: ${ZOD}
cmo: ${CMO}
cmi: ${CMI}

define ignore
if test "${DEBUG}" = "yes"; then
  echo "Ignoring $1$2."; fi
touch .$1$2.ign
case "$2" in
  \.mli) rm -f $1.cmi;;
   \.ml) rm -f $1.cmo $1.cmx;;
esac
endef

# Normalising dependencies

define norm_dep
dep_mod=$$(sed "s/.*:\(.*\)/\1/g" $2)

for d in $$dep_mod; do
  found=
  for m in ${ALL_MODS}; do
    if test "$$d" = "$$m"; then found=$$d; break
    else new_d=$$(echo $$d | sed "s/\<./\l&/g")
         if test "$$new_d" = "$$m"; then
           found=$$new_d; break; fi; fi; done
  if test -n "$$found"; then dep="$$dep $$found"
  else missing="$$missing $$d"; fi
done

missing=$$(echo $$(printf '%s\n' $$missing | sort -u))
sed ${I} "/.*: $1$$/d" .ext 2>/dev/null

for lib in $$missing; do
  echo "$$lib: $1" >> .ext
  ext=$$(echo $$lib | sed "s/\<./\l&/g")
  case $$ext in
     lwt_daemon|lwt_gc|lwt_io|lwt_log|lwt_main\
    |lwt_engine|lwt_process|lwt_throttle|lwt_timeout\
    |lwt_lwt_unix|lwt_bytes|lwt_sys) \
      packs="$$packs lwt.unix";;
    lwt_react) \
      packs="$$packs lwt.react";;
    lwt_preemptive) \
      packs="$$packs lwt.preemptive";;
    lwt_ssl) \
      packs="$$packs lwt.ssl";;
    lwt_glib) \
      packs="$$packs lwt.glib";;
    opium) \
      packs="$$packs opium.unix";;
    menhirLib) \
      packs="$$packs menhirLib";;
    *) intf=$$(find ${LIB_PATH} -name $$ext.cmi -or -name $$lib.cmi)
       matches=$$(echo "$$intf" | wc -l)
       if test "$$matches" -gt "1"; then
         printf "Error: At least two libraries contain the same .cmi:\n$$intf\n"
         exit 1; fi
       if test -n "$$intf"; then
         intf_path=$$(dirname $$intf)
         intf_name=$$(basename $$intf .cmi)
         pack=$$(basename $$intf_path)
         if test "$$pack" = "ocaml"; then
           case $$lib in
             Num|Big_int|Arith_status)
               if test -z "$$nums"; then
                 packs="$$packs nums"
                 nums=yes; fi;;
             Thread|Mutex|Condition|Event|ThreadUnix)
               if test -z "$$threads"; then
                 packs="$$packs threads"
                 threads=yes; fi;;
             UnixLabels)
               if test -e $$intf_path/unix.cma; then
                 packs="$$packs unix"; fi;;
             Stdlib);;
             *) if test -e $$intf_path/$$intf_name.cma; then
                  packs="$$packs $$intf_name"; fi;;
           esac
         elif test "$$pack" != "compiler-libs"
         then packs="$$packs $$pack"; fi; fi;;
  esac
done

packages=$$(echo $$packs | tr ' ' '\n' | sort -u | sed '/^\s*$$/d')
if test -n "$$packages" -a -n "${OCAMLFIND}"; then
  if test "${DEBUG}" = "yes"; then
    printf "Extracting packages required by $1... "; fi
  echo "$$packages" > .$1.pack
#  echo "$$packages" >> .pack
#  sort -u -o .pack .pack
  if test "${DEBUG}" = "yes"; then echo "done."; fi; fi

if test -e .ext; then sort -u -o .ext .ext; fi
if test "${DEBUG}" = "yes"; then cp -f $2 $2.old; fi

  echo $$dep \
| sed -e "s/\>/.cmi/g" -e "s/^/$1: /g" \
      -e "s/\.mli/.cmi/g; \
          s/^\(.*\)\.ml: \(.*\)/\1.cmo \1.cmx: \2 \1.cmi/g" \
      -e "s/ \+/ /g" \
      -e "s/\([^ ]*\)\.cmi \1\.cmi$$/\1.cmi/g" > $2
endef

# Extracting dependencies with ocamldep

define mk_dep
base=$$(basename $<)
${if ${DEBUG},echo "Entering mk_dep ($$base)."}
pre=$$(basename $$base ${suffix $<})
tag=${SRCDIR}/.$$base.tag
src=${SRCDIR}/$$base
syn=.$$base.syn
ign=.$$base.ign
sem=.$$base.sem
wrn=.$$base.wrn

if echo ${IMPL_ONLY} | grep -w $$pre > /dev/null 2>&1; then
  syn=.$$pre.mli.syn; fi

if test -n "$$(find -L $$syn -newer $$src 2>/dev/null)"; then
  if test -e $$tag; then
    if test -n "$$(find -L $$syn -newer $$tag 2>/dev/null)"; then
      up=no; fi
  else up=no; fi; fi

if test "$$up" = "no"; then
  if test ! -e $$syn.dis; then
    ${call display,$$syn}
  elif test "yes" = "${VERB}"; then
    echo "Ignoring $$base."; fi
else
  flags="$$(sed -n 's/^ocamldep: \(.*\)/\1/p' $$tag 2>/dev/null)"
  ${if ${VERB},printf "Extracting dependencies of $$base... "}
  ${strip ${OCAMLFIND} \
            ${OCAMLDEP} ${DFLAGS} $$flags $$src 2> $$syn > $@}
  if test -s $$syn; then
    rm -fr $@
    sed ${I} 's|${SRCDIR}/||' $$syn
    if grep -q "^Error" $$syn > /dev/null 2>&1
    then ${if ${VERB},${call failed,:}}
         sed ${I} "/^[ ]*$$/d" $$syn
    else ${if ${VERB},echo "done:";} ok=yes
    fi
    ${call display,$$syn}
    if test "$$ok" != "yes"; then
      ${call ignore,$$pre,${suffix $<}}
    fi
  else ${if ${VERB},echo "done.";} ok=yes
    sed ${I} 's|${SRCDIR}/||' $@
  fi
  if test "$$ok" = "yes"; then
    rm -f $$syn $$syn.dis
    if test -s $$src; then rm -f $$ign; fi
    ${call norm_dep,$$base,$@}
  fi
  rm -f $$sem $$wrn $$sem.dis; fi
endef

# Extracting compilation dependencies with Menhir

define mk_infer_dep
${if ${DEBUG},echo "Entering mk_infer_dep ($1.mly)."}
mly=${SRCDIR}/$1.mly
tag=${SRCDIR}/.$1.mly.tag
mli=${SRCDIR}/$1.mli
ml=${SRCDIR}/$1.ml
ml_dep=.$1.ml.dep
mli_dep=.$1.mli.dep
out=.$1.mly.out
err=.$1.mly.err
ign=.$1.mly.ign
rem=.$1.mly.rem
mli_ign=.$1.mli.ign
ml_ign=.$1.ml.ign
obj=".$1.cmi .$1.cmo .$1.cmx"

if test -n "$$(find -L $$ign -newer $$mly 2>/dev/null)"; then
  if test -e $$tag; then
    if test -n "$$(find -L $$ign -newer $$tag 2>/dev/null)"; then
      up=no; fi
  else up=no; fi; fi

if test "$$up" = "no"; then
  if test ! -e $$err.dis -a -e $$err; then
    ${call display,$$err}
    if test "${DEBUG}" = "yes"; then
      echo "Ignoring $1.mly."; fi; fi
  touch $$mli $$mli_ign $$ml $$ml_ign $$ign
  rm -f $$obj
else
  if test "${DEBUG}" = "yes"; then
    printf "Changing directory to ${notdir ${SRCDIR}}.\n"; fi
  ${if ${VERB},printf "Extracting dependencies of $1.mly... "}
  flags="$$(echo $$(cat $$tag 2>/dev/null))"
  cd ${SRCDIR}
  ${strip ${MENHIR} --raw-depend \
                    --ocamldep="${OCAMLDEP} ${DFLAGS}" \
                    ${YFLAGS} $$flags $1.mly > ${OBJDIR}/$$out 2>&1}
  if test "$$?" = "0"; then
    if test "${DEBUG}" = "yes"; then \
      printf "\nChanging directory to ${notdir ${OBJDIR}}.\n"; fi
    cd ${OBJDIR}
    ${if ${VERB},printf "done.\n"}
    sed -n "/^$1.ml:/p"  $$out > $$ml_dep
    sed -n "/^$1.mli:/p" $$out > $$mli_dep
    # echo "ml_dep:"
    # cat $$ml_dep
    # echo "mli_dep:"
    # cat $$mli_dep
    rm -f $$err $$err.dis $$ign
    ${call norm_dep,$1.ml,$$ml_dep}
    # Circular dependency:
    # sed ${I} "s/^/$1.ml /g" $$ml_dep
    ${call norm_dep,$1.mli,$$mli_dep}
    sed ${I} "s/^/$1.mli /g" $$mli_dep
    # [menhir --raw-depend --ocamldep="..."] does not generate a
    # dependency on menhirLib for $$ml_dep (it does for $$mli_dep).
    if test -e .$1.mli.pack; then
      cp -f .$1.mli.pack .$1.ml.pack; fi
    if test -e .$1.mli.lib; then
      cp -f .$1.mli.lib  .$1.ml.lib; fi
    if test -e .$1.mli.incl; then
      cp -f .$1.mli.incl .$1.ml.incl; fi

    if test "${DEBUG}" = "yes"; then mv -f $$out $$out.old
    else rm -f $$out; fi
  else
    cd ${OBJDIR}
    if test "${VERB}" != "yes"; then
      printf "Extracting dependencies of $1.mly... "; fi
    ${call failed,:}
    mv $$out $$err
    ${call display,$$err}
    rm -f $$mli $$ml
    touch $$mli $$mli_ign $$ml $$ml_ign $$ign $$err.dis
    rm -f $$obj
    echo "Ignoring $$mly."; fi; fi
endef

.PHONY: infer_dep
infer_dep:
> for par in ${YMOD}; do ${call mk_infer_dep,$$par}; done

# Administration of compilation dependencies

define mv_metadata
${if ${VERB},printf "Reassigning metadata of $*.ml to $*.mli... "}
if test -e .$*.ml.syn; then mv -f .$*.ml.syn .$*.mli.syn; fi
if test -e .$*.ml.sem; then mv -f .$*.ml.sem .$*.mli.sem; fi
if test -e .$*.ml.wrn; then mv -f .$*.ml.wrn .$*.mli.wrn; fi
if test -e .$*.ml.ign; then mv -f .$*.ml.ign .$*.mli.ign; fi

if test -e .$*.ml.syn.dis; then mv -f .$*.ml.syn.dis .$*.mli.syn.dis; fi
if test -e .$*.ml.sem.dis; then mv -f .$*.ml.sem.dis .$*.mli.sem.dis; fi
${if ${VERB},echo "done.";} true
endef

define rm_metadata
${if ${VERB},printf "Removing metadata of $1... "}
rm -f .$1.syn .$1.sem .$1.wrn .$1.ign
rm -f .$1.syn.dis .$1.sem.dis
${if ${VERB},echo "done."}
endef

define forge_dep
if test -e $<; then
  if test "yes" = "${VERB}"; then
    printf "Forging dependencies for $*.mli... "; fi
  sed -n 's/^$*.cmo $*.cmx:\(.*\) $*.cmi$$/$*.cmi:\1/p' $< > $@
  if test "yes" = "${VERB}"; then echo "done."; fi
  ${call mv_metadata}; fi
endef

ifeq (yes,${LOG_OBJ})
.%.dep: ;
else
  ifeq (yes,${NO_DEP})
.%.dep: ;
  else
    ifeq (1,${MAKE_RESTARTS})
.%.dep: ;
    else
# Untagged sources

${UNTAGGED_DEP}: .%.dep: %
> ${call mk_dep}

${UNTAGGED_IMPL_ONLY:%=.%.mli.dep}: .%.mli.dep: .%.ml.dep
> ${call forge_dep}

# Tagged sources

${YMOD:%=.%.ml.dep}: .%.ml.dep: %.mly .%.mly.tag
> ${call mk_infer_dep,$*}

${YMLI_DEP}: .%.mli.dep: .%.ml.dep ;

${TAGGED_NO_Y_DEP}: .%.dep: % .%.tag
> ${call mk_dep}

${TAGGED_IMPL_ONLY:%=.%.mli.dep}: .%.mli.dep: .%.ml.dep .%.ml.tag
> ${call forge_dep}

    endif
  endif
endif

# Derived dependencies

SED_IMPL := ${foreach impl,${IMPL},s/${impl}\.cmi/${impl}.cmx/g;}

${MOD:%=.%.ml.odp} ${IMPL_ONLY:%=.%.ml.odp}: .%.ml.odp: .%.ml.dep
> if test -e $<; then \
    sed -e "s/^$*.cmo $*.cmx:\(.*\)/$*.cmx:\1/g" \
        -e "${SED_IMPL}" -e "s/\.cmx$$/.cmi/g" $< > $@; fi

ifeq (yes,${LOG_OBJ})
.%.zod: ;
else
${MOD:%=.%.ml.zod} ${IMPL_ONLY:%=.%.ml.zod}: .%.ml.zod: .%.ml.dep
> if test -e $<; then \
    if test "${VERB}" = "yes"; then \
      printf "Making $@ from $<... "; fi; \
    sed -e "s/^$*.cmo $*.cmx:\(.*\) $*.cmi/$*.cmx:\1/g" \
        -e "s/\.cmi/.cmx/g" $< > $@; \
    if test "${VERB}" = "yes"; then \
      printf "done.\n"; fi; fi
endif

# ====================================================================
# Compilation

BFLAGS ?=
OFLAGS ?= ${BFLAGS}

define compile
sem=.$1.sem
src=${SRCDIR}/$1
tag=${SRCDIR}/.$1.tag
rm -f .$1.wrn
printf "Compiling $1"

if test "$2" = ".cmi"; then rm -f $*.cmo $*.cmx; fi

if test -s .$1.pack; then
  packages=$$(echo $$(cat .$1.pack) | tr ' ' ',')
  packages="-package $$packages"; fi

if test -s .$1.incl; then
  includes=$$(sed -n "s/^/-I /p" .$1.incl | tr '\n' ' '); fi

case "$2" in
  \.cmi|\.cmo)
    printf "... "
    flags="$$(sed -n 's/^ocamlc: \(.*\)/\1/p' $$tag 2>/dev/null)"
    if test "${TRACE}" = "yes"; then
      echo "${notdir ${OCAMLFIND}} ${notdir ${OCAMLC}} \
              ${BFLAGS} $$flags $$includes $$packages \
              -c $1" | tr -s ' ' >> build.sh; fi
    ${strip ${OCAMLFIND} ${OCAMLC} \
                           ${BFLAGS} $$flags $$includes $$packages \
                           -c -o $@ $$src > $$sem 2>&1}; \
    ret_code=$$?;;
  \.cmx)
    printf " to native code... "
    flags=""
    if test -e $$tag; then
      if grep "^ocamlopt:" $$tag > /dev/null 2>&1
      then flags="$$(sed -n 's/^ocamlopt: \(.*\)/\1/p' $$tag)"
      else flags="$$(sed -n 's/^ocamlc: \(.*\)/\1/p' $$tag)"; fi; fi
    if test "${TRACE}" = "yes"; then
      echo "${notdir ${OCAMLFIND}} ${notdir ${OCAMLOPT}} \
              ${OFLAGS} $$flags $$includes $$packages \
              -c $1" | tr -s ' ' >> build.sh; fi
    ${strip ${OCAMLFIND} ${OCAMLOPT} \
                           ${OFLAGS} $$flags $$includes $$packages \
                           -c -o $@ $$src > $$sem 2>&1}; \
    ret_code=$$?;;
esac

sed ${I} 's|${SRCDIR}/||' $$sem

lines="$$(wc -l $$src | sed 's/ *\([0-9]\+\) .*/\1/g')"
if test -s $$sem; then
  sed ${I} "/^[ ]*$$/d" $$sem
  if test -n "$$(grep -qi "^Error\|^Fatal error" $$sem > /dev/null 2>&1)" \
          -o $$ret_code != 0
  then ${call failed,:}
       ${call display,$$sem}
       ${call ignore,${basename $1},${suffix $1}}
  else printf "done"
       ${call line_count,$$lines}; echo
       mv -f $$sem .$1.wrn
       rm -f .$1.ign $$sem.dis; fi
else printf "done"; ${call line_count,$$lines}; echo
     rm -f $$sem .$1.ign $$sem.dis; fi
endef

# Determining the prerequisites of $^ that entail a recompilation

define chk_dep
dep="${notdir ${filter-out %.ml,${^:%.cmi=%}}}"
for mod in $$dep; do
  if test -e .$$mod.mli.ign; then
    skip=$$mod; break
  elif test -n "$$(find $$mod.cmi -newer .$1.sem 2>/dev/null)"
    then updates="$$mod $$updates"
  elif test -n "$$(find $$mod.cmi -newer .$1.wrn 2>/dev/null)"
    then updates="$$mod $$updates"; fi
done
endef

# Compiling implementations having interfaces,
# or interfaces without implementations

define comp_unit
if test "${suffix $@}" = ".cmi"; then ext=mli; else ext=ml; fi
unit=$*.$$ext
${if ${DEBUG},echo "Entering comp_unit ($$unit)."}

src=${SRCDIR}/$$unit
tag=${SRCDIR}/.$$unit.tag
mly=${SRCDIR}/$*.mly

sem=.$$unit.sem
mll_err=.$*.mll.err
mly_err=.$*.mly.err

if test ! -e .$$unit.syn; then
  if test -e $$mll_err; then
    if test -e $$mll_err.dis; then
      echo "Ignoring $$unit."
    else ${call display,$$mll_err}; fi
  elif test -e $$mly_err; then
    if test -e $$mly_err.dis; then
      echo "Ignoring $$unit."
    else ${call display,$$mly_err}; fi
  else
    ${call chk_dep,$$unit}
    if test -n "$$skip"; then
      ${call ignore,$*,.$$ext}
    else
      if test -z "$$updates"; then
        if test -n "$$(find -L $$sem -newer $$src 2>/dev/null)"; then
          if test -e $$tag; then
            if test -n "$$(find -L $$sem -newer $$tag 2>/dev/null)"
            then up=no; fi
          else up=no; fi; fi; fi
      if test "$$up" = "no"; then
        if test -e $$sem.dis; then echo "Ignoring $$unit."
        else ${call display,$$sem}; fi
      elif test -e $$mly -a "$$ext" = "ml"; then
        if test -s .$$unit.pack; then
          packages=$$(echo $$(cat .$$unit.pack) | tr ' ' ',')
          packages="-package $$packages"; fi
        if test -s .$$unit.incl; then
          includes=$$(sed -n "s/^/-I /p" .$$unit.incl | tr '\n' ' '); fi
        cd ${SRCDIR}
        flags="$$(sed -n 's/^ocamlc: \(.*\)/\1/p' $$tag 2>/dev/null)"
        camlcmd="${OCAMLFIND} ${OCAMLC} -I ${OBJDIR} ${BFLAGS} $$flags $$includes $$packages"
        if test "${TRACE}" = "yes"; then
          echo "camlcmd=\"${notdir ${OCAMLFIND}} ${notdir ${OCAMLC}} -I ${OBJDIR} ${BFLAGS} $$flags $$includes $$packages\"" >> ${OBJDIR}/build.sh
        fi
        ${call mk_par,$*,--infer --ocamlc="$$camlcmd",,infer}
        cd ${OBJDIR}
        if test ! -e $$sem; then
          ${call compile,$$unit,${suffix $@}}; fi
      else ${call compile,$$unit,${suffix $@}}; fi; fi; fi; fi

if test -e .$$unit.ign
then echo $$unit >> .${BIN}.ign
else sed ${I} "/^$$unit/d" .${BIN}.ign 2>/dev/null; true; fi
endef

# Compiling implementations without interfaces

define comp_stand
${if ${DEBUG},echo "Entering comp_stand ($@: $*.ml)."}
ml=${SRCDIR}/$*.ml
tag=${SRCDIR}/.$*.ml.tag
sem=.$*.mli.sem
ignored=

if test ! -e .$*.mli.syn; then
  if test -e .$*.mll.err; then
    if test -e $$err.dis; then
      echo "Ignoring $*.ml."
      ignored=$*.ml
    else ${call display,$$err}; fi
  else
    ${call chk_dep,$*.mli}
    if test -n "$$skip"; then
      ${call ignore,$*,.ml}
      ignored=$*.ml
      mv .$*.ml.ign .$*.mli.ign
    else
      if test -z "$$updates"; then
        if test -n "$$(find -L $$sem -newer $$ml 2>/dev/null)"; then
          if test -e $$tag; then
            if test -n "$$(find -L $$sem -newer $$tag 2>/dev/null)"; then
              up=no; fi
          else up=no; fi; fi; fi
      if test "$$up" = "no"; then
        if test -e $$sem.dis; then
          echo "Ignoring $*.ml."
          ignored=$*.ml
        else ${call display,$$sem}; fi
      else case "$1" in
             \.cmo) alt=$*.cmx;;
             \.cmx) alt=$*.cmo;;
           esac
           ${MAKE} -f ${SRCDIR}/${THIS} MAKEFLAGS=-Rrsiq \
                   NO_DEP:=yes $$alt
           if test "$$?" = "0"; then
             mv $*.cmi $*.cmi.old
             ${if ${VERB},echo "Saved $*.cmi."}
             ${call compile,$*.ml,$1}
             mv -f $*.cmi.old $*.cmi
             ${if ${VERB},echo "Restored $*.cmi."}
           else ${call compile,$*.ml,$1}
           fi
           ${call rm_metadata,$*.mli}
           ${call mv_metadata}; fi; fi; fi; fi

if test -e .$*.mli.ign; then
  if test -z "$$ignored"; then
    echo "Ignoring $*.ml."; fi
  echo $*.ml >> .${BIN}.ign
else
  sed ${I} "/^$*.ml/d" .${BIN}.ign 2>/dev/null; true; fi
endef

ifeq (no,${LOG_OBJ})
export NATIVE := ${filter %.opt nat opt %.cmx,${MAKECMDGOALS}}

# Interfaces

${INTF_ONLY:%=%.cmi} ${MOD:%=%.cmi}: %.cmi: .%.mli.dep
> ${call comp_unit}

${IMPL_ONLY:%=%.cmi}: %.cmi: .%.ml.dep
> ${call comp_stand,${if ${NATIVE},.cmx,.cmo}}

%.cmi: ;

# Implementations (bytecode)

${MOD:%=%.cmo}: %.cmo: .%.ml.dep
> @${call comp_unit}

${IMPL_ONLY:%=%.cmo}: %.cmo: %.cmi
> if test -e $@ -a $< -nt $@ \
     -o ! -e $@ -a ! -e .$*.mli.ign; \
  then ${call comp_stand,.cmo}; fi

%.cmo: ;

# Implementations (native code)

${MOD:%=%.cmx}: %.cmx: %.ml
> ${call comp_unit}

${IMPL_ONLY:%=%.cmx}: %.cmx: %.cmi
> if test -e $@ -a $< -nt $@ \
     -o ! -e $@ -a ! -e .$*.mli.ign; \
  then ${call comp_stand,.cmx}; fi

%.cmx: ;

# Compiling C files through [ocamlc]

CCOPT := ${addprefix -ccopt ,${CFLAGS}}

define compile_C
src=${notdir $^}
printf "Compiling $$src..."
err=.$$src.err
if test "${TRACE}" = "yes"; then
  echo "${notdir ${OCAMLFIND}} \
          ${notdir ${OCAMLC}} -I $$(${OCAMLC} -where) -c ${notdir $^} ${CCOPT}" \
| tr -s ' ' >> build.sh; fi
${OCAMLFIND} ${OCAMLC} -I $$(${OCAMLC} -where) -c $^ ${CCOPT} > $$err 2>&1
if test -s $$err; then
  ${call failed,:}
  ${call display,$$err}
else rm -f $$err
     lines="$$(wc -l ${SRCDIR}/$$src \
               | sed 's/ *\([0-9]\+\) .*/\1/g')"
     printf " done"
     ${call line_count,$$lines}; echo; fi
endef

%.o: %.c
> ${call compile_C}

else # infer a linking order:
${INTF_ONLY:%=%.cmi}: %.cmi: ;
%.cmi: %.cmx ;

%.cmx: FORCE
> if test -e ${SRCDIR}/$*.ml -o -e ${SRCDIR}/$*.mly; then \
    echo $* >> .${BIN}.lnk; \
    if test "${DEBUG}" = "yes"; then \
      echo "Added module $* to .${BIN}.lnk."; fi; fi

endif

# ====================================================================
# Linking dependencies

del = ${SRCDIR}/$1.mli .$1.mli.syn .$1.mli.sem .$1.mli.wrn \
      .$1.mli.ign $1.cmi \
      ${SRCDIR}/$1.ml .$1.ml.syn .$1.ml.sem .$1.ml.wrn .$1.ml.ign \
      $1.cmo $1.cmx

define clean_stubs
${if ${VERB},printf "Removing stubs (if any)... "}
for parser in ${YMOD}; do
  if test -e .$$parser.mly.err; then
    rm -f ${call del,$$parser}; fi
done
for lexer in ${LMOD}; do
  if test -e .$$lexer.mll.err; then
    rm -f ${call all_ml,$$lexer}
    if test ! -e ${SRCDIR}/$$lexer.mli; then
      rm -f ${call all_mli,$$lexer}; fi; fi
done
${if ${VERB},echo "done."}
endef

clean_stubs:
> ${call clean_stubs}

define clean_displays
${if ${VERB},printf "Removing displays (if any)... "}
rm -f .*.dis
${if ${VERB},echo "done."}
endef

clean_displays:
> ${call clean_displays}

# ====================================================================
# Warnings

define print_warn
warnings=$$(ls .*.wrn 2>/dev/null); \
message="> In directory ${OBJDIR}, check warning"; \
\
if test "$$(ls .*.wrn 2>/dev/null | wc -l)" = "1"; \
  then /bin/echo -e "\e[31m$$message $$warnings\e[0m"; \
elif test -n "$$warnings"; then \
  /bin/echo -e "\e[31m$${message}s:\e[0m"; \
  ls $$warnings; fi
endef

.PHONY: warn
warn: ; @${call print_warn}

# ====================================================================
# Linking

define update_links
circ=.${BIN}.circ
cp /dev/null .${BIN}.lnk
if test "${DEBUG}" = "yes"
then printf "Updating linking order for ${BIN}...\n"
else printf "Updating linking order for ${BIN}... "
fi

${MAKE} -f ${SRCDIR}/${THIS} ${IMPL:%=-W %.ml} \
        ${BIN}.cmx LOG_OBJ:=yes 2> $$circ
if test "$$?" = "0"; then echo "done."
else echo "FAILED."; rm -f .*.dis; exit 1; fi

if test -s $$circ; then
  sed ${I} -e "s/.*: //g" \
           -e "s/\.cmx <-/.mli <-/g" \
           -e "s/<- \([^ ]*\)\.cmx/<- \1.ml/g" \
           $$circ
else rm -f $$circ; fi
endef

.${BIN}.lnk: ${ZOD} .del
> ${if ${DEBUG},echo "Outdated prerequisites: $?"}
> for prereq in $?; do \
    if test -e $$prereq; then up=yes; break; fi; done
> if test "$$up" = "yes"; then ${call update_links}; fi

define prelink
lnk=.${BIN}.lnk
${if ${DEBUG},echo "Entering prelink to make $@."}
# cp -f $$lnk $$lnk.old 2>/dev/null

${MAKE} -f ${SRCDIR}/${THIS} NO_DEP:=yes $$lnk
if test "$$?" != "0"; then rm -f .*.dis; exit 1; fi

if test -s .${BIN}.ign; then
  sed "s/^\(.*\)\.ml[i]\?$$/\1/" .${BIN}.ign \
| sort -u \
| while read mod; do sed ${I} "/^$$mod$$/d" $$lnk 2>/dev/null; done
fi
if grep -w ${BIN} $$lnk > /dev/null 2>&1; then
#  if ! diff -q $$lnk $$lnk.old > /dev/null 2>&1; then
    ${MAKE} -f ${SRCDIR}/${THIS} $1 NO_DEP:=yes \
            OBJ:="$$(echo $$(cat $$lnk))" $@
    if test "$$?" != "0"; then rm -f .*.dis; exit 1; fi
#  fi
else ${call emphasise,Error: Cannot link objects to build $@:}
     ${call emphasise,Main module ${BIN} is faulty.}
     rm -f .*.dis; exit 1
fi
endef

define link_err
${call display,$$err}
if test -s .${BIN}.circ; then ${call display,.${BIN}.circ}; fi
if test "${origin OBJ}" != "command line"; then
  echo "> Check OBJ in Makefile.cfg (order)."; fi
rm -f .*.dis; exit 1
endef

define link
tag=${SRCDIR}/.${BIN}.tag
rm -f $@
${if ${DEBUG},echo "Entering link with objects \`${strip ${OBJ}}'."}
err=.$@.err
wrn=.$@.wrn
skip=
for mod in ${OBJ}; do
  if test -e .$$mod.mli.ign -o -e .$$mod.ml.ign; then
    skip=$$mod; break; fi
  if test -n "$$(find $$mod.$1 -newer $$err 2>/dev/null)"; then
    updates="$$mod $$updates"; fi
done
if test -n "$$skip"; then
  ${call emphasise,Error: Cannot link objects to build $@:}
  ${call emphasise,Module $$skip is faulty.}
  rm -f .*.dis; exit 1
elif test -e $$err -a -z "$$updates" -a $$tag -ot $$err; then
  ${call link_err}
else
  if test "$1" = "cmo"; then tool=ocamlc; else tool=ocamlopt; fi
  flags=$$(sed -n "s/^$$tool: \(.*\)/\1/p" $$tag 2>/dev/null)

  if test "${origin OBJ}" = "command line"
  then objects="${OBJ}"; skip=no
  else for obj in ${OBJ}; do
         if test -s $$obj.$1; then
           objects="$$objects $$obj"
         elif test "$$obj" = "${BIN}"; then
           skip=yes; break; fi
       done
  fi
  objects="${EXTRA_OBJ} $$objects"
  if test -z "$$objects" -o "$$skip" = "yes"
  then printf "Linking objects as $@... "
       ${call failed,: Cannot link objects to build $@.}
       rm -f .*.dis; exit 1
  else
    if test -n "${OCAMLFIND}"; then
      all_pack=$$(echo $$objects \
                  | sed -e "s/\</./g" -e "s/\>/.ml.pack/g")
      for p in $$all_pack; do
        if test -f "$$p"; then pack="$$p $$pack"; fi
      done
      if test -n "$$pack"; then
        packages=$$(cat $$pack | sort -u)
        packages=$$(echo $$packages | tr ' ' ',')
        packages="-package $$packages -linkpkg"; fi; fi

    all_incl=$$(echo $$objects \
                | sed -e "s/\</./g" -e "s/\>/.ml.incl/g")
    for incl in $$all_incl; do
      if test -f "$$incl"; then includes="$$incl $$includes"; fi
    done
    if test -n "$$includes"; then
      includes=$$(cat $$includes | sort -u)
      includes=$$(echo $$includes | sed -n "s/^/-I /p" | tr '\n' ' ')
    fi

    all_libs=$$(echo $$objects \
                | sed -e "s/\</./g" -e "s/\>/.ml.lib/g")
    for lib in $$all_libs; do
      if test -f "$$lib"; then libs="$$lib $$libs"; fi; done
    if test -n "$$libs"; then
      libraries=$$(cat $$libs | sort -u)
      libraries=$$(echo $$libraries | sed "s/\>/.$1/g" | tr '\n' ' ')
    fi

    cm=$$(echo $$objects | sed "s/\>/.$1/g")

    link_opt="$$flags $$packages $$includes -o $@ $$libraries $$cm"

    if test "${DEBUG}" = "yes"; then
      if test -n "$$flags" -o -n "$$packages" -o -n "$$includes"; then
        printf "\nFlags for linking: \`%s %s %s'.\n" \
               "$$flags" "$$packages" "$$includes"; fi; fi
    rm -f $$err $$wrn
    printf "Linking objects as $@... "
    case "$1" in
      cmo) if test "${TRACE}" = "yes"; then
             echo "${notdir ${OCAMLFIND}} \
                     ${notdir ${OCAMLC}} $$link_opt" \
           | tr -s ' ' >> build.sh; fi
           $$(eval ${strip ${OCAMLFIND} \
                     ${OCAMLC} $$link_opt > $$err 2>&1});;
      cmx) if test "${TRACE}" = "yes"; then
             echo "${notdir ${OCAMLFIND}} \
                     ${notdir ${OCAMLOPT}} $$link_opt" \
           | tr -s ' ' >> build.sh; fi
           $$(eval ${strip ${OCAMLFIND} \
                     ${OCAMLOPT} $$link_opt > $$err 2>&1});;
    esac
    if test -s $$err; then
      sed ${I} -e '/"_none_"/d' -e 's/Error: //g' $$err
      if grep -qi "Warning" $$err; then
        echo "done."
        mv $$err $$wrn
      else ${call failed,:}; ${call link_err}; fi
    else echo "done."; rm -f $$err; fi; fi; fi
endef

# Linking bytecode

TAG := ${notdir ${wildcard ${SRCDIR}/.${BIN}.tag}}

ifndef OBJ
${BIN}.byte: FORCE
> rm -f .${BIN}.ign
> if test -e .${BIN}.byte -o ! -e .${BIN}.opt; then \
    flags="MAKEFLAGS=-Rrsj -Oline"; fi
> ${MAKE} -f ${SRCDIR}/${THIS} $$flags NO_DEP:=yes ${BIN}.cmo; \
  if test "$$?" != "0"; then rm -f .*.dis; exit 1; fi
> ${call prelink,$$flags}
> rm -f .${BIN}.opt
> touch .${BIN}.byte
> if test -z "${SESSION}"; then \
    ${MAKE} -f ${SRCDIR}/${THIS} close_session NO_DEP:=yes; \
    if test "$$?" != "0"; then rm -f .*.dis; exit 1; \
    elif test "${DEBUG}" != "yes"; then rm -f .*.old; fi; fi
else
${BIN}.byte: ${OBJ:%=%.cmo} ${C:%.c=%.o} ${TAG} .del
> ${call link,cmo}
endif

# Linking native code

ifndef OBJ
${BIN}.opt: FORCE
> rm -f .${BIN}.ign
> if test -e .${BIN}.opt -o ! -e .${BIN}.byte; then \
    flags="MAKEFLAGS=-Rrsj -Oline"; fi
> ${MAKE} -f ${SRCDIR}/${THIS} $$flags NO_DEP:=yes ${BIN}.cmx; \
  if test "$$?" != "0"; then rm -f .*.dis; exit 1; fi
> ${call prelink,$$flags}
> rm -f .${BIN}.byte
> touch .${BIN}.opt
> if test -z "${SESSION}"; then \
    ${MAKE} -f ${SRCDIR}/${THIS} close_session NO_DEP:=yes; \
    if test "$$?" != "0"; then rm -f .*.dis; exit 1; \
    elif test "${DEBUG}" != "yes"; then rm -f .*.old; fi; fi
else
${BIN}.opt: ${OBJ:%=%.cmx} ${C:%.c=%.o} ${TAG} .del
> ${call link,cmx}
endif

# ====================================================================
# Installation

.PHONY: install
install: ${BIN}.opt
> if test -z "${PREFIX}"; then \
    ${call emphasise,"Error: Set variable PREFIX."}; \
  else printf "Installing $^ at ${PREFIX}... "; \
       install $^ ${PREFIX}; \
       if test "$$?" = "0"; then echo "done."; \
       else ${call failed,.}; fi; fi

endif

# ====================================================================
# Closing a session (printing warnings, cleaning stubs and displays)

.PHONY: close_session
close_session:
> ${call print_warn}
> ${call clean_stubs}
> ${call clean_displays}

else
# ====================================================================
# We are in the source directory ${SRCDIR}

#SRCDIR = ${CURDIR}
VPATH = ${OBJDIR}

# Macros identifying some metadata and generated sources

all_mli  = $1.mli ${addprefix ${OBJDIR}/,${call from_mli,$1}}
all_ml   = $1.ml ${addprefix ${OBJDIR}/,${call from_ml,$1}}
from_mll = ${OBJDIR}/.$1.mll.err ${call all_ml,$1}
from_mly = ${OBJDIR}/.$1.mly.err \
           ${call all_ml,$1} ${call all_mli,$1} \
           $1.output $1.automaton $1.conflicts

# Filtering targets

.DEFAULT_GOAL := all

ifeq (,${MAKECMDGOALS})
BUILD := ${.DEFAULT_GOAL}
else
BUILD := ${filter %.cmi %.cmo %.cmx %.byte %.opt \
                  %.mli %.ml .%.dep .%.lnk \
                  all byte nat opt install dep odp zod sync cmo cmi \
                  conc infer_dep, \
                  ${MAKECMDGOALS}}
endif

export BUILD

FILTER_SRC := false

ifeq (0-,${MAKELEVEL}-${MAKE_RESTARTS})
  ifneq (,${BUILD})
    FILTER_SRC := true
  else
    ifneq (,${filter %.dep msg raw src clean lines,${MAKECMDGOALS}})
      FILTER_SRC := true
    endif
  endif
endif

# Filtering sources and tags

ifeq (true,${FILTER_SRC})
export C            := ${wildcard *.c}
export MLL          := ${wildcard *.mll}
export MLY          := ${wildcard *.mly}
export TAGS         := ${wildcard .*.tag}
export TAGGED_MLY   := ${filter ${MLY}, \
                         ${patsubst .%.mly.tag, %.mly, ${TAGS}}}
export TAGGED_MLL   := ${filter ${MLL}, \
                         ${patsubst .%.mll.tag, %.mll, ${TAGS}}}
export TABLED_MLY   := ${if ${TAGGED_MLY}, \
                         ${patsubst .%.mly.tag, %.mly, \
                           ${shell grep -l -e "--table" \
                                        ${TAGGED_MLY:%=.%.tag}}}}
export MSG          := ${TAGGED_MLY:%.mly=%.msg}
export RAW          := ${addsuffix .raw,${MSG}}
export MSG_NEW      := ${addsuffix .new,${MSG}}
export MSG_ML       := ${patsubst %.mly, %_msg.ml, ${TABLED_MLY}}
export LMOD         := ${basename ${MLL}}
export YMOD         := ${basename ${TAGGED_MLY}}
export LML          := ${LMOD:%=%.ml}
export YMLI         := ${YMOD:%=%.mli}
export YML          := ${YMOD:%=%.ml}
export MLI          := ${wildcard *.mli}
export INTF         := ${sort ${YMOD} ${basename ${MLI}}}
export CMI          := ${INTF:%=%.cmi}
export ML           := ${wildcard *.ml} ${wildcard Version.ml}
export IMPL         := ${sort ${LMOD} ${YMOD} ${basename ${ML} ${MSG_ML}}}
export INTF_ONLY    := ${filter-out ${IMPL},${INTF}}
export IMPL_ONLY    := ${filter-out ${INTF},${IMPL}}
export MOD          := ${filter ${INTF},${IMPL}}
export ROOTS        := ${filter ${IMPL},${TAGS:.%.tag=%}}
export ALL_MODS     := ${sort ${INTF} ${IMPL}}
export GDEP         := ${INTF_ONLY:%=.%.mli.dep} \
                       ${IMPL_ONLY:%=.%.ml.dep} \
                       ${MOD:%=.%.mli.dep} ${MOD:%=.%.ml.dep}
YML_DEP             := ${YML:%=.%.dep}
YMLI_DEP            := ${YMLI:%=.%.dep}
NO_Y_DEP            := ${filter-out ${YML_DEP} ${YMLI_DEP},${GDEP}}
export
TAGGED_IMPL_ONLY    := ${filter ${TAGS:.%.ml.tag=%},${IMPL_ONLY}}
export
UNTAGGED_IMPL_ONLY  := ${filter-out ${TAGGED_IMPL_ONLY},${IMPL_ONLY}}
export UNTAGGED_DEP := ${filter-out ${TAGS:%.tag=%.dep} ${YMLI_DEP} ${YML_DEP}, \
                                    ${GDEP}}
export
TAGGED_NO_Y_DEP     := ${filter ${TAGS:%.tag=%.dep},${NO_Y_DEP}}

CORE_FILES := \
  ${strip ${MLY} ${MLL} ${filter-out ${MSG_ML} ${LML} ${YML},${ML}} \
          ${INTF_ONLY:%=%.mli}}

ALL_FILES := ${strip ${CORE_FILES} ${filter-out ${YMLI},${MLI}} ${C}}

${if ${DEBUG},${info Filtered sources and tags.}}
endif

# Checking for executable targets

ifeq (,${filter ${CLEANING} conc msg raw infer_dep %.dep \
                src warn lines phony size env conf cmi cmo, \
                ${MAKECMDGOALS}})
  ifeq (,${ROOTS})
    NO_VERSION := ${filter-out Version, ${IMPL}}
    ifeq (${words ${NO_VERSION}},1)
ROOTS := ${NO_VERSION}
    else
      ${error No tagged executable found. \
              Check tag files and implementations}
    endif
  endif
endif

export OPT  := ${addsuffix .opt,${ROOTS}}
export BYTE := ${addsuffix .byte,${ROOTS}}

# Main goals and default

.PHONY: all byte opt nat

all byte: sync
> ${MAKE} -f ${THIS} ${BYTE} SESSION:=yes; \
  if test "$$?" != "0"; then rm -f .*.dis; exit 1; fi
> ${MAKE} -f ${THIS} close_session NO_DEP:=yes

opt nat: sync
> ${MAKE} -f ${THIS} ${OPT} SESSION:=yes; \
  if test "$$?" != "0"; then rm -f .*.dis; exit 1; fi
> ${MAKE} -f ${THIS} close_session NO_DEP:=yes

# Checking that there are no <foo>.mll and <foo>.mly

OVERLAP := ${filter ${YMOD},${LMOD}}
ifneq (,${OVERLAP})
  OVERLAP := ${shell echo ${OVERLAP} | sed "s/\<./\u&/g;s/ /, /g"}
  ifeq (${words ${OVERLAP}},1)
  ${error Module ${OVERLAP} cannot be generated by both \
          [ocamllex] and [menhir]}}
  else
  ${error Modules ${OVERLAP} cannot be generated by both \
          [ocamllex] and [menhir]}}
  endif
endif

# Checking that there are no <foo>.ml and <foo>.c

OVERLAP := ${filter ${IMPL},${basename ${C}}}
ifneq (,${OVERLAP})
  OVERLAP := ${shell echo ${OVERLAP} | sed "s/\<./\u&/g;s/ /, /g"}
  ifeq (${words ${OVERLAP}},1)
  ${error Object for module ${OVERLAP} cannot be generated by both \
          [ocamlopt] and a C compiler}}
  else
  ${error Objects for modules ${OVERLAP} cannot be generated by both \
          [ocamlopt] and a C compiler}}
  endif
endif

# Creating the build and test directories

ifeq (0-,${MAKELEVEL}-${MAKE_RESTARTS})
  ifeq (,${filter ${CLEANING} size lines,${MAKECMDGOALS}})
    ${shell mkdir -p ${OBJDIR}}
    ${if ${DEBUG},${info Created build directory ${OBJDIR}.}}
    ${shell mkdir -p ${TSTDIR}}
    ${if ${DEBUG},${info Created test directory ${TSTDIR}.}}
    SYNGEN     := ${TSTDIR}/syntax/generated
    TABLED_MOD := ${TABLED_MLY:%.mly=%}
    export TABLED_DIRS := ${foreach mod,${TABLED_MOD},${SYNGEN}/${mod}}
    ${foreach dir,${TABLED_DIRS},${shell mkdir -p ${dir}}}
  endif
endif

# Defining and checking executable targets

BYTE_TARGETS := ${filter %.byte,${MAKECMDGOALS}}
OPT_TARGETS  := ${filter %.opt,${MAKECMDGOALS}}

INVALID_BYTE_TARGETS := ${filter-out ${BYTE},${BYTE_TARGETS}}
INVALID_OPT_TARGETS  := ${filter-out ${OPT},${OPT_TARGETS}}
INVALID_TARGETS := ${strip ${INVALID_BYTE_TARGETS} ${INVALID_OPT_TARGETS}}

ifeq (${words ${INVALID_BYTE_TARGETS}},1)
  BASE_TARGET := ${patsubst %.byte,%,${INVALID_BYTE_TARGETS}}
  ${error Cannot build target ${INVALID_BYTE_TARGETS}. \
          Check .${BASE_TARGET}.tag and ${BASE_TARGET}.ml{,l,y}}
else ifeq (${words ${INVALID_OPT_TARGETS}},1)
  BASE_TARGET := ${patsubst %.opt,%,${INVALID_OPT_TARGETS}}
  ${error Cannot build target ${INVALID_OPT_TARGETS}. \
          Check .${BASE_TARGET}.tag and ${BASE_TARGET}.ml{,l,y}}
else ifneq (,${INVALID_TARGETS})
  ${error Cannot build targets ${INVALID_TARGETS}. \
          Check tag files and implementations}
endif

# Including project-wide configuration

sinclude Makefile.cfg

# The parser generator is Menhir

MENHIR ?= menhir

# Jumping to the build directory and updating recursively

define goto_build
if test -d "${OBJDIR}"; then \
  if test "${DEBUG}" = "yes"; then \
    echo "Changing directory to ${notdir ${OBJDIR}}."; fi; \
  ${MAKE} -f ${CURDIR}/${THIS} --no-print-directory \
          -C ${OBJDIR} SRCDIR=${CURDIR} $2 $1; \
  if test "$$?" != "0"; then rm -f ${OBJDIR}/.*.dis; exit 1; fi; fi
endef

# TEMPORARY
.PHONY: src
src: ${LML} ${YMLI} ${YML} ${MSG_ML}

%.byte: ${if ${SESSION},,sync} src FORCE
> ${call goto_build,$@,BIN=$*}

%.opt: ${if ${SESSION},,sync} src FORCE
> ${call goto_build,$@,BIN=$*}

infer_dep dep odp zod: src FORCE
> ${call goto_build,$@}

%.dep: FORCE
> ${call goto_build,$@}

.PHONY: cmo
cmo: dep FORCE
> ${call goto_build,$@,CMO=${addsuffix .cmo,${ROOTS}}}

%.cmo: FORCE
> ${call goto_build,$@}

.PHONY: cmi
cmi: dep FORCE
> ${call goto_build,$@}

%.cmi: FORCE
> ${call goto_build,$@}

# Checking system configuration (for debugging purposes)

CMD := "ocamlc ocamlc.opt ocamlopt ocamlopt.opt ocamldep \
        ocamldep.opt ocamllex ocamllex.opt menhir ocamlfind \
        ocamlobjinfo ocamlwc camlp4 camlp5 grep sed arch"

define chk_cfg
IFS=':'
for cmd in $1; do
  found=no
  for dir in $$PATH; do
    if test -z "$$dir"; then dir=.; fi
    if test -x "$$dir/$$cmd"; then found=$$dir; break; fi
  done
  if test "$$found" = "no"; then
    echo "Shell command $$cmd not found."
  else echo "Found $$found/$$cmd"; fi
done
endef

.PHONY: conf
conf: ; @${call chk_cfg,"${CMD}"}

# Synchronise the build state / Display warnings

sync warn: ; ${call goto_build,$@}

# Updating the linking dependencies (for debugging)

.%.lnk: ; @${call goto_build,$@,BIN=$* LOG_OBJ=yes}


# ====================================================================
# Generating lexers with ocamllex

%.mll %.mly: ;

LFLAGS ?=

define mk_lex
${if ${DEBUG},echo "Entering mk_lex ($<)."}
mll=$<
ml=$@
base=$$(basename $$mll .mll)
obj="${OBJDIR}/.$$base.cmo ${OBJDIR}/.$$base.cmx"
tag=.$$mll.tag
out=${OBJDIR}/.$$mll.out
err=${OBJDIR}/.$$mll.err
wrn=${OBJDIR}/.$$mll.wrn
rem=${OBJDIR}/.$$mll.rem
ign=${OBJDIR}/.$$ml.ign
src=${OBJDIR}/.src

if test -n "$$(find -L $$err -newer $$mll 2>/dev/null)"; then
  if test -e $$tag; then
    if test -n "$$(find -L $$err -newer $$tag 2>/dev/null)"; then
      up=no; fi
  else up=no; fi; fi

if test "$$up" = "no"
then ${call display,$$err}
     if test "${DEBUG}" = "yes"; then echo "Ignoring $$ml."; fi
     touch $$ml $$ign $$err.dis
     rm -f $$obj
else rm -f ${call from_mll,$*}
     lines="$$(wc -l $$mll | sed 's/ *\([0-9]\+\) .*/\1/g')"
     printf "Making $$ml from $$mll"
     ${call line_count,$$lines}; printf ".. "
     flags="$$(echo $$(cat $$tag 2>/dev/null))"
     if test "${TRACE}" = "yes"; then
       echo "${notdir ${OCAMLLEX}} ${LFLAGS} $$flags $$mll" \
     | tr -s  ' ' >> ${OBJDIR}/build.sh; fi
     ${strip ${OCAMLLEX} ${LFLAGS} $$flags $$mll > $$out 2>&1}
     if test "$$?" = "0"; then
       echo "done:"
       echo $$ml >> $$src
       sort -u -o $$src $$src
     else ${call failed,:}
     fi

     cp /dev/null $$err
     cp /dev/null $$wrn
     cp /dev/null $$rem

     while read line; do
       if echo "$$line" | grep "^File .*:" > /dev/null 2>&1; then
         if test -n "$$acc"; then acc="$$acc\n$$line"
         else acc="$$line"; fi
       elif echo "$$line" | grep "^File " > /dev/null 2>&1; then
         if test -n "$$acc"; then echo "$$acc\n$$line" >> $$err
         else echo "$$line" >> $$err; fi
         acc=
       elif echo "$$line" | grep "^Error" > /dev/null 2>&1; then
         if test -n "$$acc"; then echo "$$acc\n$$line" >> $$err
         else echo "File \"$$mll\":\n$$line" >> $$err; fi
         acc=
       elif echo "$$line" | grep "^Warning" > /dev/null 2>&1; then
         if test -n "$$acc"; then echo "$$acc\n$$line" >> $$wrn
         else echo "File \"$$mll\":\n$$line" >> $$wrn; fi
         acc=
       elif test -n "$$acc"; then
         acc="$$acc\n$$line"
       else echo "$$line" >> $$rem; fi
     done < $$out

     rm -f $$out
     if test -n "$$acc"; then echo "$$acc" >> $$err; fi

     if test -s $$err; then
       rm -f $$obj
       ${call display,$$err}
       if test "${DEBUG}" = "yes"; then echo "Ignoring $$ml."; fi
       touch $$ml $$ign $$err.dis
     else
       cat $$rem
       rm -f $$err $$err.dis
     fi
     rm -f $$rem
     if test ! -s $$wrn; then rm -f $$wrn; fi; fi
endef

${filter-out ${TAGGED_MLL:%.mll=%.ml},${LML}}: %.ml: %.mll
> ${call mk_lex}

${TAGGED_MLL:%.mll=%.ml}: %.ml: %.mll .%.mll.tag
> ${call mk_lex}

# ====================================================================
# Generating parsers with Menhir

# If there is only one implementation and this implementation is
# generated from a parser specification, it does not require a tag to
# be processed by Menhir; otherwise, a tag is required.

ifeq (,${TAGGED_MLY})
  ifeq (${words ${MLY}},1)
${MLY:%.mly=%.mli}: %.mli: %.mly
> ${call mk_par,$*,${YFLAGS} -la 1,,}
  endif
else
${filter-out ${TAGGED_MLY:%.mly=%.mli},${YMLI}}: %.mli: %.mly ;

${TAGGED_MLY:%.mly=%.mli}: %.mli: %.mly .%.mly.tag \
  $${shell grep -o '\<\S\+\.mly\>' .$$*.mly.tag | tr '\n' ' '}
> ${call mk_par,$*,${YFLAGS} -la 1,$?,}
endif

${YML}: %.ml: %.mli ;

# Generating error messages with Menhir

define mk_msg
mly=$1.mly
msg=$1.msg
conflicts=$1.conflicts
out=${OBJDIR}/.$$mly.out
err=${OBJDIR}/.$$msg.err

if test -e $$msg; then mv -f $$msg $$msg.old; echo "Saved $$msg."; fi

printf "Making new $$msg from $$mly... "
flags="$$(echo $$(cat .$$mly.tag 2>/dev/null))"
${strip ${MENHIR} --list-errors ${YFLAGS} $$flags $$mly > $$msg 2> $$out}

if test "$$?" = "0"; then
  if test -e $$conflicts; then rm -f $$conflicts; fi
  sentences=$$(grep "YOUR SYNTAX ERROR MESSAGE HERE" $$msg | wc -l)
  if test -z "$$sentences"; then printf "done.\n"
  else
    spurious=$$(grep WARNING $$msg | wc -l)
    printf "done:\n"
    printf "There are %s error sentences, %s with spurious reductions.\n" \
           $$sentences $$spurious
  fi
  sed ${I} 's/ *$$//' $$msg
  if test -s $$out; then cat $$out; fi
  if test -f $$msg.old; then
    printf "Checking inclusion of mappings (new in old)... "
    ${strip ${MENHIR} --compare-errors $$msg \
                      --compare-errors $$msg.old \
                      ${YFLAGS} $$flags $$mly 2> $$out}

    if test "$$?" = "0"; then
      if test -s $$out; then
        printf "done:\n"
        cat $$out
      else printf "done.\n"; fi
      rm -f $$out
      printf "Updating $$msg... "
      ${strip ${MENHIR} --update-errors $$msg.old \
                        ${YFLAGS} $$flags $$mly \
                        > $$msg 2> $$err}
      sed ${I} 's/ *$$//' $$msg
      if test "$$?" = "0"; then
        if $$(diff $$msg $$msg.old 2>&1 > /dev/null); then
          echo "done."
        else
          printf "done:\n"
          ${call emphasise,Warning: The LR items may have changed.}
          ${call emphasise,> Check your error messages again.}
        fi
        rm -f $$err
        if test -e $$conflicts; then rm -f $$conflicts; fi
      else ${call failed,"."}
           touch $$err
           mv -f $$msg.old $$msg
           echo "Restored $$msg."; fi
    else ${call failed,:}
         mv -f $$out $$err
         sed ${I} -e "s/\.msg/.msg.new/g" \
                  -e "s/\.new\.old//g" $$err
         mv -f $$msg $$msg.new
         ${call emphasise,> See $$err and update $$msg.}
         echo "The default messages are in $$msg.new."
         mv -f $$msg.old $$msg
         echo "Restored $$msg."; fi; fi
else
  ${call failed,:}
  mv -f $$out $$err
  ${call emphasise,> See $$err.}
  mv -f $$msg.old $$msg
  echo "Restored $$msg."
fi
endef

${TABLED_MLY:%.mly=%.msg}: %.msg: %.mly #.%.ml.dep
> if test -e ${OBJDIR}/.$*.mly.ign; then \
    if test "${DEBUG}" = "yes"; then \
      echo "Ignoring $@."; fi; \
  else ${call mk_msg,$*}; fi

.PHONY: msg
msg:
> for par in ${YMOD}; do ${call mk_msg,$$par}; done

# Generating source code from error messages

define mk_msg_ml
${if ${DEBUG},echo "Entering mk_msg_ml ($@)."}
msg_err=${OBJDIR}/.$*_msg.err
if test -e ${OBJDIR}/.$*.ml.ign \
     -o -e ${OBJDIR}/.$*.msg.err; then
  echo "Ignoring $*_msg.ml."
  touch $*_msg.ml ${OBJDIR}/.$*_msg.mli.ign
  rm -f ${OBJDIR}/$*_msg.cmo ${OBJDIR}/$*_msg.cmx
else
  flags="$$(echo $$(cat .$*.mly.tag 2>/dev/null))"
  printf "Making $*_msg.ml from $*.msg... "
  ${strip ${MENHIR} --compile-errors $*.msg ${YFLAGS} \
                    $$flags $*.mly > $*_msg.ml 2> $$msg_err}
  if test "$$?" = "0"
  then printf "done.\n"
       rm -f ${OBJDIR}/.$*_msg.mli.ign
       if test -e $*.conflicts; then rm -f $*.conflicts; fi
  else ${call failed,:}
       ${call display,$$msg_err}
       if test "${DEBUG}" = "yes"; then
         echo "Ignoring $*_msg.mli."; fi
       touch $*_msg.ml ${OBJDIR}/.$*_msg.mli.ign
       rm -f ${OBJDIR}/$*_msg.cmi \
             ${OBJDIR}/$*_msg.cmo \
             ${OBJDIR}/$*_msg.cmx; fi
  rm -f $$msg_err; fi
endef

${TABLED_MLY:%.mly=%_msg.ml}: %_msg.ml: %.msg
> ${call mk_msg_ml,$*}

# Producing erroneous sentences from Menhir's error messages

define mk_raw
raw=${OBJDIR}/$1.msg.raw
printf "Making $1.msg.raw from $1.msg... "
flags="$$(echo $$(cat .$1.mly.tag 2>/dev/null))"
${strip ${MENHIR} --echo-errors $1.msg ${YFLAGS} \
                  $$flags $1.mly > $$raw 2>/dev/null}
sed ${I} -e 's/^.*: \(.*\)$$/\1/g' $$raw
printf "done.\n"
endef

.PHONY: raw
raw:
> for par in ${YMOD}; do ${call mk_raw,$$par}; done

${RAW}: %.msg.raw: %.msg
> ${call mk_raw,$*}

# Converting Menhir's minimal erroneous sentences to concrete syntax

define mk_conc
printf "Unlexing the erroneous sentences... "
for dir in ${TABLED_DIRS}; do
  msg=$$(basename $$dir).msg
  states=${OBJDIR}/$$msg.states
  map=${OBJDIR}/$$msg.map
  raw=${OBJDIR}/$$msg.raw
  sed -n "s/.* state\: \([0-9]\+\)./\1/p" $$msg > $$states
  paste -d ':' $$states $$raw > $$map
  rm -f $$dir/*.${EXT}
  while read -r line; do
    state=$$(echo $$line | sed -n 's/\(.*\):.*/\1/p')
    filename=$$(printf "$$dir/%04d.${EXT}" $$state)
    sentence=$$(echo $$line | sed -n 's/.*:\(.*\)/\1/p')
    echo $$sentence | ${OBJDIR}/unlexer.opt >> $$filename
  done < $$map
done
printf "done.\n"
endef

.PHONY: conc
ifndef EXT
conc:
> echo "Please set the file extension with EXT (without a period)."
else
conc: ${RAW} unlexer.opt
> ${call mk_conc}
endif

# ====================================================================
# Miscellanea

.PHONY: env
env:
> echo "OCAMLFIND=[${OCAMLFIND}]"
> echo "OCAMLC=[${OCAMLC}]"
> echo "OCAMLOPT=[${OCAMLOPT}]"
> echo "OCAMLDEP=[${OCAMLDEP}]"
> echo "OCAMLLEX=[${OCAMLLEX}]"
> echo "MENHIR=[${MENHIR}]"

.PHONY: phony
phony:
> sed -n 's/^\.PHONY: \(.*\)$$/\1/p' ${THIS} | tr ' ' '\n' | sort

.PHONY: lines
lines:
ifeq (,${ALL_FILES})
> echo "No source files."
else
> ocamlwc -c ${CORE_FILES}
> semi=$$(grep "^[ ]*;;[ ]*$$" ${CORE_FILES} | wc -l)
> echo "Lines with two semicolons: $$semi"
endif

.PHONY: size
size:
> sed -e "/^[[:space:]]*#/d" -e "/^[[:space:]]*$$/d" ${THIS} | wc -l

# ====================================================================
# Cleaning the slate (Add your own [clean::] rules in [Makefile.cfg].)

mostlyclean:
> rm -f ${LML} ${YMLI} ${YML} ${MSG_NEW} ${RAW} ${MSG_ML}
> rm -f *.output *.automaton *.conflicts
> ${call goto_build,$@}

clean:: mostlyclean
> rm -fr ${OBJDIR}

clean_obj clean_bin clean_stubs clean_dep clean_displays close_session:
> ${call goto_build,$@}

endif
