#!/bin/sh

# This script sets or unsets symbolic links specified in .links files,
# following our convention, and performs check to assess their
# validity.
#
# Author: Christian Rinderknecht

#set -x

# =====================================================================
# General Settings

quiet=no
debug=no

script=$(basename $0)

# =====================================================================
# Wrappers for several kind of displays
#
print_nl () { if test "$quiet" != "yes"; then echo "$1"; fi }

print () { if test "$quiet" != "yes"; then printf "$1"; fi }

fatal_error () {
  echo "$script: fatal error:"
  echo "$1" 1>&2
  exit 1
}

warn () {
 print_nl "$script: warning:"
 print_nl "$1"
}

verb () { if test -n "$verbose_opt"; then printf "$1"; fi }

verb_nl () { if test -n "$verbose_opt"; then echo "$1"; fi }

# =====================================================================
# Help
#
usage () {
  cat <<EOF
Usage: $(basename $0) -s [-h][-q | -v][-d][-r][<path>]
       $(basename $0) -u [-h][-q | -v][-d][-r][<path>]

Set up or unset the symbolic links in file \`<path>/.links', of
which lines are either of the form <destination path> <source filename>,
interpreted as a symbolic link from <source filename> to
<destination path>, or #include "<file>", where <file> is a file
specifying symbolic links as <destination path> <source filename> on
each line.

The <source filename> can be omitted, in which case the name of
the link will be the basename of <destination path> (following
\`ln' convention). In case <source filename> is specified, it
should be syntactically a file name, so the link is created in
the directory <path>, otherwise a warning would be issued.

If \`<path>/.links' is not found, a warning is issued and nothing
is done. If <path> is omitted, it is defaulted to \`.'

The following options, if given, must be given only once.

Display control:
  -h, --help       display this help and exit
  -q, --quiet      do not print any kind of message
  -v, --verbose    display more information (even if -q is set)

One of the following options is mandatory:
  -s, --set        set the symlinks specified in \`<path>/.links'
  -u, --unset      unset the symlinks specified in \`<path>/.links'

Other options:
  -r, --recursive  recursively descends from <path> included
  -d, --debug      display configuration of tools
EOF
  exit 4
}

# =====================================================================
# Parsing loop
#
while : ; do
  case "$1" in
    "") break;;
      # Help
      #
    -h | --help | -help)
      help=yes
      help_opt=$1
      ;;
      # Display more information
      #
    -v | --verbose)
      verbose=yes
      verbose_opt=$1
      ;;
      # Quiet
      #
    -q | --quiet)
      quiet=yes
      quiet_opt=$1
      ;;
      # Recursive descent
      #
    -r | --recursive)
      if test -n "$recursive_opt"
      then fatal_error "Repeated option --recursive (-r)."
      fi
      recursive=yes
      recursive_opt=$1
      ;;
      # Set up the symbolic links
      #
    -s | --set)
      if test -n "$SET"
      then fatal_error "Repeated option --set (-s)."
      fi
      SET=yes
      SET_opt=$1
      ;;
    -u | --unset)
      if test -n "$UNSET"
      then fatal_error "Repeated option --unset (-u)."
      fi
      UNSET=yes
      UNSET_opt=$1
      ;;
    -d | --debug)
      debug=yes
      debug_opt=$1
      ;;
      # Invalid option
      #
    -*)
      fatal_error "Invalid option \"$1\"."
      ;;
      # The path
      #
     *)
      if test -n "$path_arg"
      then fatal_error "Only one path allowed."
      fi
      path=yes
      path_arg=$1
      ;;
  esac
  shift
done

# =====================================================================
# Checking the command-line options and arguments and applying some of
# them.

# First, we check if the user asks for help.
#
if test "$help" = "yes"; then usage; fi

# We check is either --set or --unset has been given.
#
if test "$SET" != "yes" -a "$UNSET" != "yes"
then fatal_error "Option --set or --unset is mandatory."
fi

# Checking the given directory path
#
if test -z "$path_arg"
then path=.
elif test ! -d $path_arg
  then warn "Path \"$path_arg\" is not a directory."
       path=
  else path=$path_arg
fi

# =====================================================================
# Setting/unsetting the symbolic links, if any, only in the current
# directory
#
apply () {
  if test "$1" != "."
  then print_nl "*** Entering $1"
  fi
  if test -e "$1/.links"
  then if test "$verbose" != "yes"
       then print "$2 and checking..."
       fi
         cpp $1/.links \
       | sed -E '/#.*|^$/d' \
       | while read LINE; do
           static_dest=$(echo $LINE | awk '{print $1}')
           src=$(echo $LINE | awk '{print $2}')
           if test -n "$src"
           then if test "$src" != "$(basename $src)"
                then warn "Link name $src (source) is not a filename."
                fi
           fi
           dest=$(eval echo $static_dest)
           if test -n "$dest"
           then if test -n "$src"
                then verb "$2 $1/$src to $dest..."
                     if test "$2" = "linking"
                     then ln -fsn $dest $1/$src
                          verb_nl " done."
                          check_link.sh $1/$src
                     else rm -f $1/$src
                          verb_nl " done."
                     fi
                else verb "$2 $1/$(basename $dest) to $dest..."
                     if test "$2" = "linking"
                     then ln -fsn $dest $1/$(basename $dest)
                          verb_nl " done."
                          check_link.sh $1/$(basename $dest)
                     else rm -f $1/$(basename $dest)
                          verb_nl " done."
                     fi
                fi
           fi
         done
       if test "$verbose" != "yes"
       then print_nl " done."
       fi
  else warn "File \"$1/.links\" not found."
  fi
}

# =====================================================================
# Main

if test "$SET" = "yes"; then mode=linking; else mode=unlinking; fi

if test -n "$path"
then if test "$recursive" = "yes"
     then symbolic_links=$(find $path -name .links | tr '\n' ' ')
       for symbolic_link in $symbolic_links; do
         subdir=$(dirname $symbolic_link)
         apply $subdir $mode
       done
     else apply $path $mode
     fi
fi
