#!/bin/bash

MYLANG=tony
MYCOMP=tonyc

function die () {
  printf "FAILED!!!\n"
  rm -f *.asm a.*
  read -p "Press <ENTER> to continue...  "
  exit 1
}

while [ "$1" != "" ]; do
  f="${1/%.$MYLANG}"

  echo "--------------------------------------------------------------------"
  printf "%-40s" "$f"
  rm -f *.asm a.*
  cp -f "$f".$MYLANG a.$MYLANG
  ./$MYCOMP a.$MYLANG || die
  printf "success\n"

  dosbox run.bat >& /dev/null

  shift
done

rm -f *.asm a.*

exit 0
