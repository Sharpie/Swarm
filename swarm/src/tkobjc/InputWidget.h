// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// may not be right.. attempt at making a widget that's inputtable.
// the current linking scheme is pretty bad: we use Tcl's variable
// linking code to directly bind the C variable to the Tcl variable.
// Four types are allowed by TCL:
//   TCL_LINK_INT, TCL_LINK_DOUBLE, TCL_LINK_BOOLEAN, TCL_LINK_STRING

#import <tkobjc/Widget.h>
#import <gui.h>

@interface InputWidget: Widget <_InputWidget>
{
  const char *variableName;
}

- (const char *)getValue;
- linkVariableInt: (int *)p;
- linkVariableDouble: (double *)p;
- linkVariableBoolean: (BOOL *)p;
- setValue: (const char *)v;

@end
