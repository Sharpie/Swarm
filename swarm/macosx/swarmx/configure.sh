#!/bin/sh

cd $BUILT_PRODUCTS_DIR/

export MACOSX_DEPLOYMENT_TARGET=10.2

../../../configure \
CC=/usr/local/gcc3.3/bin/gcc \
--prefix=/usr/local/swarm2.1 \
--with-tcldir=/usr/local/tcl8.4.1 \
--with-tkdir=/usr/local/tk8.4.1 \
--with-bltdir=/usr/local/blt2.4z \
--with-pngdir=/usr/local/png_1.2.5 \
--without-jdkdir \
--disable-static \
--disable-fast-install \
--with-hdf5dir=/usr/local/hdf5_1.4.5
