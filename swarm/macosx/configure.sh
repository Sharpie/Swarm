#!/bin/sh

cd $BUILT_PRODUCTS_DIR/

export MACOSX_DEPLOYMENT_TARGET=10.3

# Edit this configure command to reflect local 
# library installation paths.
# Optimise for G4 or G5 by adding stuff like -fast
# -mcpuG5 -mtuneG5 etc to CFLAGS. (not tested!)

../../configure CC="gcc-4.0" CFLAGS=" -g -O0 -Wno-long-double" \
--prefix=/usr/local/swarm2.2 \
--with-ffidir=/usr/lib/gcc/powerpc-apple-darwin/4.0.0 \
--with-tcldir=/usr/local/tcl8.4.7 \
--with-tkdir=/usr/local/tcl8.4.7 \
--with-bltdir=/usr/local/tcl8.4.7 \
--with-pngdir=/usr/local/png_1.2.5 \
--with-hdf5dir=/usr/local/hdf5_1.6.1 \
--with-jdkdir=/System/Library/Frameworks/JavaVM.framework \
--enable-onelib \
--disable-fast-install \
--disable-static 

#CFLAGS="-Wno-long-double" \  Apple compilers only
#--with-jdkdir=/System/Library/Frameworks/JavaVM.framework \
#--with-ffidir=/usr/lib/gcc/powerpc-apple-darwin/4.0.0 \
#--with-ffcalldir=/Users/billn/Public/Swarm/ffcall-1.9/avcall/build/dst \
#--disable-onelib \
#--with-tcllibname=libtcl8.4g \
#--with-tclscriptdir=/usr/local/tcl8.4.5/lib/tcl8.4 \
#--with-tklibname=libtk8.4g \
#--with-ffcalldir=/Groups/billn/Public/Swarm/ffcall-1.9/avcall/build/dst \
#--without-hdf5dir \
