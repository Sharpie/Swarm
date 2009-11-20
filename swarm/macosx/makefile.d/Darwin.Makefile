OS_VERSION := $(shell sw_vers -productVersion | sed 's/\(.*\)\.[0-9]*/\1/')

SDK     := macosx${OS_VERSION}
#BASE   := swarm/macosx/SwarmOSX
BASE    := SwarmOSX
PRODUCT := ${HOME}/Library/Frameworks/Swarm.framework

include makefile.d/$(OS)-$(OS_VERSION).Makefile
