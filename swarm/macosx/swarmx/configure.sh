#!/bin/sh

cd $BUILT_PRODUCTS_DIR/

export MACOSX_DEPLOYMENT_TARGET=10.2

# Edit this configure command to reflect local 
# library installation paths.
# Optimise for G4 or G5 by adding stuff like -fast
# -mcpuG5 -mtuneG5 etc to CFLAGS. (not tested!)

../../../configure CC=cc \
CFLAGS="-ggdb -O2 -Wno-long-double" \
--prefix=/usr/local/swarm2.2p \
--with-tcldir=/usr/local/tcl8.4.4 \
--with-tkdir=/usr/local/tk8.4.4 \
--with-bltdir=/usr/local/blt2.4z \
--with-pngdir=/usr/local/png_1.2.5 \
--with-hdf5dir=/usr/local/hdf5_1.4.5p2 \
--with-jdkdir=/System/Library/Frameworks/JavaVM.framework \
--disable-fast-install \
--enable-onelib \
--disable-static 

# --disable-onelib \
# --without-jdkdir \
