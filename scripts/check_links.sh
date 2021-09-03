#!/bin/sh

# This script checks the validity of symbolic links following the
# conventions of our build architecture.
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
# Help
#
usage () {
  cat <<EOF
Usage: $(basename $0) [-h][-d][-r][<path>]

Check the validity of symbolic links in <path>.

The following options, if given, must be given only once.

Display control:
  -h, --help       display this help and exit

Other options:
  -r, --recursive  recursively descends from <path> included
  -d, --debug      display configuration of tools
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
      # Recursive descent
      #
    -r | --recursive)
      if test -n "$recursive_opt"
      then fatal_error "Repeated option --recursive (-r)."
      fi
      recursive=yes
      recursive_opt=$1
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

# Checking the given directory path
#
if test -z "$path_arg"
then path=.
elif test ! -d $path_arg
  then fatal_error "Path \"$path_arg\" is not a directory."
  else path=$path_arg/
fi

# =====================================================================
# Main
#
if test "$recursive" = "yes"
then find $path -type l -exec check_link.sh {} \;
else find $path -maxdepth 1 -type l -exec check_link.sh {} \;
fi
