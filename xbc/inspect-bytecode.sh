#!/usr/bin/env bash
set -euo pipefail

# expect to be run from root of repo

t=tmp # fixed temp dir local to invoking dir, it'll do!

repo=/home/nic/daml #discover automtically from here
jar=$repo/bazel-bin/xbc/xbc.jar
class=xbc/Native$.class

mkdir $t
(cd $t; cat $jar | jar -x $class)
find $t -name *.class | xargs javap -p -v > xbc/bytecode.text
rm -rf $t
