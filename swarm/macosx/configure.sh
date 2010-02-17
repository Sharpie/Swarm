#!/bin/sh

# cd $BUILT_PRODUCTS_DIR/  for older Xcode
cd $BUILD_DIR/

export MACOSX_DEPLOYMENT_TARGET=10.6

# Edit this configure command to reflect local 
# library installation paths.
# Optimise for G4 or G5 by adding stuff like -fast
# -mcpuG5 -mtuneG5 etc to CFLAGS. (not tested!)

# Add in locations for utilities needed for configure
export PATH=$PATH:/usr/local/bin

../../configure JAVAC=/usr/bin/javac \
 CC="gcc -B ../../tools" CFLAGS=" -g -Os -fnested-functions" \
--prefix=/Library/Frameworks/Swarm.framework/Versions/3.0 \
--with-tcldir=/Library/Frameworks/Tcl.framework/Versions/8.4 \
--with-tkdir=/Library/Frameworks/Tk.framework/Versions/8.4-X11 \
--with-bltdir=/Library/Frameworks/Tk.framework/Versions/8.4-X11 \
--with-pngdir=/Library/Frameworks/Swarm.framework/Versions/Current/Resources \
--with-ffidir=/Library/Frameworks/Swarm.framework/Versions/Current/Resources \
--with-hdf5dir=/Library/Frameworks/Swarm.framework/Versions/Current/Resources \
--without-jdkdir \
--enable-onelib \
--disable-fast-install \
--disable-static 

# --enable-openstep \
#CFLAGS="-Wno-long-double" \  Apple compilers only
#--with-jdkdir=/System/Library/Frameworks/JavaVM.framework \
#--with-ffidir=/usr/lib/gcc/powerpc-apple-darwin/4.0.0 \
#--with-ffcalldir=/Users/billn/Public/Swarm/ffcall-1.9/avcall/build/dst \
#--with-jdkdir=/System/Library/Frameworks/JavaVM.framework \
#--disable-onelib \
#--with-tcllibname=libtcl8.4g \
#--with-tclscriptdir=/usr/local/tcl8.4.5/lib/tcl8.4 \
#--with-tklibname=libtk8.4g \
#--with-ffcalldir=/Groups/billn/Public/Swarm/ffcall-1.9/avcall/build/dst \
#--with-hdf5dir=/usr/local/hdf5_1.6.1 \



