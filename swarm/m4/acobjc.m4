# Check for Objective-C compiler.
# For now we assume this is the same as $CC.
# This only exists to call _AM_DEPENDENCIES!

# Copyright 2004 Swarm Development Group.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_DEFUN([wn_PROG_OBJC],[
test -n "$OBJC" || OBJC="$CC"
AC_SUBST(OBJC)
_AM_IF_OPTION([no-dependencies],, [_AM_DEPENDENCIES(OBJC)])
])
