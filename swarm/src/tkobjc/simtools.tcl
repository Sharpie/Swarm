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

bitmap define hide {{16 16} {
    0x00, 0x00, 0x00, 0x00, 0x04, 0x80, 0x08, 0x40, 0x90, 0x27, 0x60, 0x18,
    0x60, 0x18, 0x90, 0x24, 0x10, 0x23, 0x10, 0x23, 0x90, 0x24, 0x60, 0x18,
    0x60, 0x18, 0x90, 0x27, 0x08, 0x40, 0x04, 0x80}}
        
bitmap define special {{16 16} {
    0x00, 0x00, 0x00, 0x00, 0x04, 0x80, 0x08, 0x40, 0x90, 0x27, 0x60, 0x1b,
    0x60, 0x1b, 0x90, 0x27, 0xf0, 0x3c, 0xf0, 0x3c, 0x90, 0x27, 0x60, 0x1b,
    0x60, 0x1b, 0x90, 0x27, 0x08, 0x40, 0x04, 0x80}}

bitmap define super {{16 16} {
    0x00, 0x00, 0x00, 0x00, 0x7c, 0x08, 0x44, 0x04, 0x44, 0x7e, 0x44, 0x44,
    0x7c, 0x48, 0x00, 0x40, 0x00, 0x40, 0x00, 0x40, 0x00, 0x40, 0x7c, 0x40,
    0x44, 0x40, 0x44, 0x7e, 0x44, 0x00, 0x7c, 0x00}}

proc send_id {interp ddwin data} {
    global DDOBJ
    set DDOBJ $data
    drag&drop target $ddwin handle id
}

proc gimme_ddobj {} {
    global DDOBJ
    return $DDOBJ
}

proc sitecmd {state token} {
    if {$state} {
        $token.l configure -fg OliveDrab
    } else {
        $token.l configure -fg black
    }  
}

proc do_package {probe token} {
    set wlocal [winfo parent $token]
    set local [$probe package: $wlocal]
    if {$local != {}} {
        set label_text [$probe getId: $wlocal]
        if {[winfo children $token] == {}} {
            label $token.l -text $label_text -fg black
            pack $token.l
        } else {
            $token.l config -text $label_text -fg black
        }
        return $local
}   }

proc do_package_arg {probe arg_num token} {
    set wlocal [winfo parent $token]
    set local [$probe package: $wlocal arg: $arg_num]
    if {$local != {}} {
        set label_text [$probe getId: $wlocal arg: $arg_num]
        if {[winfo children $token] == {}} {
            label $token.l -text $label_text -fg black
            pack $token.l
        } else {
            $token.l config -text $label_text -fg black
        }
        return $local
}   }

        

