// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdarg.h>
#import <stdio.h>
#import <tkobjc/TkExtra.h>

@implementation TkExtra

int Blt_Init(Tcl_Interp *);			  // wish this were declared..

- (const char *) preInitWithArgc: (int)argc argv: (char**)argv
{
  const char *filename;
  
  filename = [super preInitWithArgc: argc argv: argv];

  // now init extras widget sets.

  if (Blt_Init(interp) == TCL_ERROR)
    {
      char *msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
      if (msg == NULL)
        msg = interp->result;
      [self error:msg];
      return NULL;				  // shouldn't get here anyway
    }
  
  [self eval: "wm withdraw ."];			  // don't map "."
  
  // general problem: better mechanism for assigning bindings.
  // note, the "%s" indirection in eval: is necessary to hide %s from eval:
  // Death to Motif lossage! Delete should delete the character to the left.
  // My precedent is emacs and xterm behaviour, indeed every bit of code that
  // Isn't Motif. If you want to delete forward, Control-d still works.
  [self eval: "%s", "
bind Entry <Delete> [bind Entry <BackSpace>]
bind Text <Delete> [bind Text <BackSpace>]
"];
  return filename;
}

- (const char *)getBltVersion
{
  const char *version_string;
  
  version_string = Tcl_GetVar (interp, "blt_version", TCL_GLOBAL_ONLY);
  if (version_string == NULL)
    version_string = Tcl_GetVar2 (interp, "blt_versions", "BLT", TCL_GLOBAL_ONLY);
  return version_string;
}

@end
