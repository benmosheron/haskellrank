#!/bin/bash

# source me for useful commands.
# just set FILE environment variable

file(){
  echo $FILE
}

compile(){
  ghc -dynamic $FILE.hs
}

run(){
  compile && ./$FILE
}