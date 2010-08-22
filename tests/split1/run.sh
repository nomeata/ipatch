#!/bin/bash

set -e

cd $(dirname $0)

if test -x ../../dist/build/ipatch/ipatch
then IPATCH=$PWD/../../dist/build/ipatch/ipatch
else IPATCH=ipatch
fi
export EDITOR="$PWD/edithunk.sh"

rm -rf tmp

cp -r before/ tmp
cd tmp
export PATH="..:$PATH"
#$IPATCH split patches/testpatch
( echo eynynpatches/featureA.patch
  echo yypatches/featureB.patch 
)| $IPATCH split patches/testpatch

perl -p -i -e 's/\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d.\d+ .\d\d\d\d/ignored date/' patches/feature?.patch
cd ..
diff -r tmp after
rm -rf tmp
echo "Test done"

