# Swarm library. Copyright © 1996-2000 Swarm Development Group.  This
# program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
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

# (manor) this is used by EZBin in order to show info about contents 
# of bins.
proc active_item_info {graph} {
  global hideOption hideYes hideNo

  $graph marker create text -coords { +Inf +Inf } -name active_info_marker -anchor ne -bg {} $hideOption $hideYes

  uplevel #0 { set foundclosest 0 }
  bind $graph <Shift-ButtonPress-1> {
    if {[%%W element closest %%x %%y closestretval -interpolate 1]} {
      set foundclosest 1
      %%W marker configure active_info_marker -text "$closestretval(x),$closestretval(y)" $hideOption $hideNo
      %%W element activate $closestretval(name) $closestretval(index)
    }
  }
  bind $graph <ButtonRelease-1> {
    if {$foundclosest} {
      %%W marker configure active_info_marker $hideOption $hideYes
      %%W element deactivate $closestretval(name)
      set foundclosest 0
    }
  }
  bind $graph <KeyPress-o> {
    %%W marker configure active_outlier_marker $hideOption $hideNo
  }
  bind $graph <KeyRelease-o> {
    %%W marker configure active_outlier_marker $hideOption $hideYes
  }
  bind $graph <Enter> {
    focus %%W
  }
}

