#!/bin/bash

cd $(dirname "$0")

for dir in *
do
	if test -d $dir
	then
		echo -n "Test $dir... "
		if $dir/run.sh >$dir/last-run.log 2>&1
		then echo "ok."
		else echo "failed!."
		fi
	fi
done

