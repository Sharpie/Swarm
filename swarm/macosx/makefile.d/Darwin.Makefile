OS_VERSION := $(shell sw_vers -productVersion | sed 's/\(.*\)\.[0-9]*/\1/')

SDK             := macosx${OS_VERSION}
#BASE           := swarm/macosx/SwarmOSX
BASE            := SwarmOSX
XCODE_TEMPLATES := "/Library/Application\ Support/Developer/Shared/Xcode/Project\ Templates/Applications"

ifeq (${PRODUCT},)
PRODUCT         := ${HOME}/Library/Frameworks/Swarm.framework
endif

Darwin_help:
	@echo "Makefile for the impatient..."
	@echo ""
	@echo "    make info"
	@echo "    make build"
	@echo "    make install"
	@echo "    make clean"
	@echo ""

Darwin_info:
	@sw_vers
	@echo ""
	@xcodebuild -showsdks
	@echo "Installation settings..."
	@echo ""
	@echo "    PRODUCT=${PRODUCT}"
	@echo ""

include makefile.d/$(OS)-$(OS_VERSION).Makefile
