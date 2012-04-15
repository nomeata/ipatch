#!/bin/bash

set -e

cd $(dirname $0)

if test -x ../../dist/build/ipatch/ipatch
then IPATCH=$PWD/../../dist/build/ipatch/ipatch
else IPATCH=ipatch
fi

rm -rf tmp

cp -r before/ tmp
cd tmp
export PATH="..:$PATH"
( echo nyyyyyy
)| $IPATCH apply patches/testpatch

cd ..
diff -Nur after tmp
rm -rf tmp
echo "Test done"

