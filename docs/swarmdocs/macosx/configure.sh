#!/bin/sh

cd $BUILT_PRODUCTS_DIR/

export MACOSX_DEPLOYMENT_TARGET=10.2

# Customise the configure command for your system
../../configure CC=cc \
CFLAGS="-ggdb -O2" \
--prefix=install_prefix \
--disable-static \
--with-swarm=path_to_Swarm_sources \
--with-catalog=path_to_catalog_for_dtd_and_xsl

## --with-javadoc=/System/Library/Frameworks/JavaVM.framework \ This still has problems
