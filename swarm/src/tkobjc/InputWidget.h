// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

// may not be right.. attempt at making a widget that's inputtable.
// the current linking scheme is pretty bad: we use Tcl's variable
// linking code to directly bind the C variable to the Tcl variable.
// Four types are allowed by TCL:
//   TCL_LINK_INT, TCL_LINK_DOUBLE, TCL_LINK_BOOLEAN, TCL_LINK_STRING

#import <tkobjc/Widget.h>
#import <gui.h>

@interface InputWidget: Widget <InputWidget>
{
  const char *variableName;
}

- (const char *)getValue;
- (void)linkVariableInt: (int *)p;
- (void)linkVariableDouble: (double *)p;
- (void)linkVariableBoolean: (unsigned *)p;
- (void)setValue: (const char *)v;
@end
