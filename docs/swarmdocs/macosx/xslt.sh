#!/bin/sh

cd $BUILD_DIR/refbook/

echo "Directories: "$BUILT_PRODUCTS_DIR" "$BUILD_DIR" "$SRCROOT

export SGML_CATALOG_FILES="../catalog"

xsltproc \
--catalogs \
-o $BUILT_PRODUCTS_DIR/html/refbook/ \
/Users/billn/Public/Swarm/docstuff/docbook-xsl-1.61.3/html/chunk.xsl \
/Users/billn/Public/Swarm/docs/swarmdocs/refbook/refbook.xml
