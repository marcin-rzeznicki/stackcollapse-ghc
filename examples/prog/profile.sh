#! /bin/sh

rebuild () {
  stack build && stack build --profile prog:exe:prog-exe
}

runProfile () {
  local prog=$1
  shift
  stack exec --profile -- prog-exe $prog +RTS -i0.001 -ki64k -po$prog -s $@
}

progName=$1
shift
extraArgs="$@"

rebuild && runProfile $progName "$extraArgs"



