#!/bin/bash

filename="$1"

if [[ -z "$filename" ]]; then
  >&2 echo 'Usage: run.sh <filename>'
  exit 1
fi

tool="${filename#*.}"

"bin/debug/$tool" "$1" > /tmp/demo.s
as --32 /tmp/demo.s -o /tmp/demo.o
ld -m elf_i386 /tmp/demo.o -o /tmp/demo
/tmp/demo
