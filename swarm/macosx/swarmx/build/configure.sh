#!/bin/sh

../../../configure \
CC='cc -fgnu-runtime -no-cpp-precomp' \
--prefix=/Users/billn/Public/Swarm/swarm/macosx/swarmx/build/dst/usr \
--with-tcldir=/usr/local/lib \
--without-jdkdir \
--with-hdf5dir=/usr/local
