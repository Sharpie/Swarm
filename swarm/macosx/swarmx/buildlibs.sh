#!/bin/sh

# See where we are
# set

# This must the path to GNU sed.  Will be done by autoconf shortly.
export SED=/usr/bin/gsed
export MACOSX_DEPLOYMENT_TARGET=10.2

cd $BUILD_DIR
make -k