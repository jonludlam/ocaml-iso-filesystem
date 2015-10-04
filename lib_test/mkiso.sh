#!/bin/bash

if type "mkisofs" > /dev/null; then
  mkdir test_iso
  echo "hello, world!" > test_iso/hello.txt
  echo "old file" > test_iso/old.txt
  touch -t 197001010001 test_iso/old.txt
  mkisofs -o test.iso -R test_iso
else
  curl http://www.recoil.org/~jon/ocaml-iso9660-test.iso -o test.iso
fi


