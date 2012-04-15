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
( echo yynnpatches/featureA.patch
  echo yynpatches/addfile.patch
  echo yypatches/removefile.patch
)| $IPATCH split patches/testpatch

perl -p -i -e 's/\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d.\d+ .\d\d\d\d/ignored date/' patches/featureA.patch patches/addfile.patch patches/removefile.patch
cd ..
diff -Nur after tmp
rm -rf tmp
echo "Test done"

