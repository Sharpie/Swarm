#!/bin/sh

cd $BUILT_PRODUCTS_DIR/

# This must the path to GNU sed.  Will be done by autoconf shortly.
export SED=/usr/bin/gsed
export MACOSX_DEPLOYMENT_TARGET=10.2

../../../configure \
CC=/usr/local/gcc3.3/bin/gcc \
--prefix=/usr/local/swarm2.1 \
--with-tcldir=/usr/local/tcl8.4.1 \
--with-tkdir=/usr/local/tk8.4.1 \
--with-bltdir=/usr/local/blt2.4z \
--without-jdkdir \
--disable-static \
--without-hdf5dir
