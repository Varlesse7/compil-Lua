#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: ./run.sh tests/prog.miniml"
  exit 1
fi

make
./main $1
