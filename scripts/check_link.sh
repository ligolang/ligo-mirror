#!/bin/sh

# This script checks the validity of one symbolic link. It is called
# by the script check_links.sh.
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
      # Invalid option
      #
    -*)
      fatal_error "Invalid option \"$1\"."
      ;;
      # The symbolic link
      #
     *)
      if test -n "$base_arg"
      then fatal_error "Only one symlink allowed."
      fi
      base=yes
      base_arg=$1
  esac
  shift
done
# =====================================================================
# Help
#
usage () {
  cat <<EOF
Usage: $(basename $0) [-h][-d] <symlink>
Check the validity of symbolic link <symlink>.

The following options, if given, must be given only once.

Display control:
  -h, --help       display this help and exit

Other options:
  -d, --debug      display configuration of tools
EOF
  exit 6
}

if test "$help" = "yes"; then usage; fi

# =====================================================================
# Main
#
if test -n "$base_arg"
then directory=$(dirname "$base_arg")
     ls_info=$(ls -l "$base_arg" 2>&1)
     if test $? -ne 0
     then fatal_error "Symbolic link not found"
     fi
     points_to=$(echo "$ls_info" | sed 's|.* -> \(.*\)|\1|g')
    (cd $directory > /dev/null
     if test ! -e "$points_to"
     then printf "Broken symlink $base_arg -> $points_to in"
         if test "$directory" = "."
         then echo " current directory."
         else echo " $directory."
         fi
     fi)
else fatal_error "Provide a symbolic link"
fi
