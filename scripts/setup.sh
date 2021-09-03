#!/bin/sh

# If our build architecture is detected, this script deploys the
# symbolic links to our makefiles, runs the autoconfiguration and
# performs some sanity checks.
#
# Author: Christian Rinderknecht

#set -x

# =====================================================================
# General Settings

# In a future release, $quiet could be passed as an option
#
quiet=no

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

# =====================================================================
# Help
#
usage () {
  cat <<EOF
Usage: $(basename $0) [-h][-q | -v][-d][-r][<path>]

Set up the symbolic links in <path>, as specified in the file
\`<path>/.links' (see script \`symlinks.sh'), run \`autconf' and
finally \`./configure'.

If <path> is omitted, it is defaulted to \`.'

The following options, if given, must be given only once.

Display control:
  -h, --help         display this help and exit
  -q, --quiet        do not print any kind of message
  -v, --verbose      display more information (even if -q is set)

Other options:
  -r, --recursive    recursively descends from <path>
  -d, --debug        display configuration of tools
EOF
  exit 1
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
    -d | --debug)
      debug=yes
      debug_opt=$1
      ;;
      # Display mode information
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

# Checking the given directory path
#
if test -z "$path_arg"
then path=.
elif test ! -d $path_arg
  then fatal_error "Path \"$path_arg\" is not a directory."
  else path=$path_arg
fi

# =====================================================================
# Main
#
apply () {
  if test "$1" != "." -o "$recursive" = "yes"
  then print_nl "*** Entering $1"
  fi

  (cd $1
   symlinks.sh -s $verbose_opt $quiet_opt
   if test -e configure.ac
   then print "autoconfiguration..."
        autoconf
        print_nl " done."
   fi
   if test -e configure
   then if test "$verbose" = "yes" -a "$recursive" != "yes"
        then ./configure
        else print "configuration..."
             ./configure --quiet > /dev/null
             print_nl " done."
        fi
   fi)
}

if test "$recursive" = "yes"
then symbolic_links=$(find $path -name .links -type f | tr '\n' ' ')
     for symbolic_link in $symbolic_links; do
       subdir=$(dirname $symbolic_link)
       apply $subdir
     done
elif test -e $path/.links
  then apply $path
fi
