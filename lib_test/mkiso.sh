#!/bin/bash

mkdir test_iso
echo "hello, world!" > test_iso/hello.txt
mkisofs -o test.iso -R test_iso

