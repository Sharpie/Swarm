# swarm.make
#
# Makefile flags and configs to build with the Swarm library
# in the GNUstep Makefile System.
#
# Copyright © 2003 Swarm Development Group.
# Author: Scott Christley
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA
# 
# The Swarm Development Group can be reached via our website at:
# http://www.swarm.org/

# Don't reload if already loaded
ifneq ($(SWARM_LOADED),yes)
SWARM_LOADED := yes

# Nothing special just include the library
AUXILIARY_LDFLAGS += -L$(SWARMHOME)/lib
AUXILIARY_TOOL_LIBS += -lswarm

# We need the default Swarm app definitions
# TODO: app name and version should not be hard-coded, better to have
# these gotten from the GNUstep classes.
AUXILIARY_INCLUDE_DIRS += -I$(SWARMHOME)/include
AUXILIARY_CPPFLAGS += -DAPPNAME=heatbugs -DAPPVERSION=2.1.1 \
		   -DBUGADDRESS=bug-swarm@swarm.org

endif
