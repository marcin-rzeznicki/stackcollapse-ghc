#! /bin/sh

buildProfile () {
  stack ghc --profile --resolver nightly-2020-09-16 \
  --package data-ordlist \
  --package pqueue \
  --package vector \
  --package deepseq \
  Main.hs -- -rtsopts -prof
}

runProfile () {
  local prog=$1
  shift
  local rts_options="-i0.001 -ki64k -po$prog -s $@"
  ./Main $prog +RTS $rts_options
}

progName=$1
shift
extraArgs="$@"

buildProfile && runProfile $progName "$extraArgs"



