#!/bin/sh

cd $BUILT_PRODUCTS_DIR/

export MACOSX_DEPLOYMENT_TARGET=10.2

../../../configure CC=cc \
CFLAGS="-ggdb -O2" \
--prefix=/usr/local/swarmdocs \
--disable-static \
--with-swarm=/Users/billn/Public/Swarm/swarm

# --disable-onelib \
# --without-jdkdir \
