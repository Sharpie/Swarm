// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <tk.h>
#include <tkobjc/TkExtra.h>

extern TkExtra *globalTkInterp;


Tk_Window 
tkobjc_nameToWindow (const char *widgetName)
{
  return Tk_NameToWindow ([globalTkInterp interp],
                          (char *)widgetName,
                          [globalTkInterp mainWindow]);
}

void
tkobjc_unlinkVar (const char *variableName)
{
  Tcl_UnlinkVar ([globalTkInterp interp], (char *)variableName); 
}

void
tkobjc_linkVar (const char *variableName, const char *p, int type)
{
  Tcl_LinkVar ([globalTkInterp interp], (char *)variableName, (char *)p, type);
}

static void
registerInterp (void)
{
  [globalTkInterp registerObject: globalTkInterp withName: "globalTkInterp"];
}

void
tkobjc_initTclInterp (int argc, const char *argv)
{
  globalTkInterp = [TclInterp alloc];  // misnomer
  [globalTkInterp initWithArgc: 1 argv: (char **)argv];
  registerInterp ();
}

void
tkobjc_initTkInterp (int argc, const char *argv)
{
  globalTkInterp = [TkExtra alloc];
  [globalTkInterp initWithArgc: 1 argv: (char **)argv];
  registerInterp ();
}
