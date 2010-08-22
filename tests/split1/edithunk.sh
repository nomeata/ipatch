#!/bin/bash

ed $1 <<__END__
11t9
wq
__END__
