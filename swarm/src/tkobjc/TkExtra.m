// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#import <tkobjc/TkExtra.h>

#include <tcl.h>

static void
ensureBltSupportFiles (id globalTkInterp)
{
  const char *fileName = "bltGraph.tcl";
  const char *basePath = [globalTkInterp globalVariableValue: "blt_library"];
  char buf[strlen (basePath) + 1 + strlen (fileName) + 1];

  strcpy (buf, basePath);
  strcat (buf, "/");
  strcat (buf, fileName);

  if (access (buf, F_OK) == -1)
    {
      fprintf (stderr, "BLT support file `%s' not found\n", fileName);
      fprintf (stderr, "If the directory `%s' not the intended location for "
               "the BLT support files,\nplease adjust BLT_LIBRARY to the "
               "right place.\n", basePath);
      exit (1);
    }
}

@implementation TkExtra

int Blt_Init(Tcl_Interp *);			  // wish this were declared..

- (const char *) preInitWithArgc: (int)argc argv: (char**)argv
{
  const char *filename;
  
  filename = [super preInitWithArgc: argc argv: argv];

  // now init extras widget sets.

  if (Blt_Init (interp) != TCL_OK)
    {
      char *msg = Tcl_GetVar (interp, "errorInfo", TCL_GLOBAL_ONLY);
      
      if (msg == NULL)
        msg = interp->result;
      [self error:msg];
      return NULL;				  // shouldn't get here anyway
    }

  ensureBltSupportFiles (self);

  // (nelson) I think this is ok: lets us load cool graph code.
  // presumably, $blt_library is always set right.
  [self eval: "lappend auto_path $blt_library"];

  [self eval: "wm withdraw ."];			  // don't map "."
  
  // general problem: better mechanism for assigning bindings.
  // note, the "%s" indirection in eval: is necessary to hide %s from eval:
  // Death to Motif lossage! Delete should delete the character to the left.
  // My precedent is emacs and xterm behaviour, indeed every bit of code that
  // Isn't Motif. If you want to delete forward, Control-d still works.
  [self eval: "%s", 
        "bind Entry <Delete> [bind Entry <BackSpace>]; "
        "bind Text <Delete> [bind Text <BackSpace>]"];
  
  if (strcmp ([self getBltFullVersion], "8.0-unoff") == 0)
    {
      [self eval: "namespace import blt::barchart"];
      [self eval: "namespace import blt::bitmap"];
      [self eval: "namespace import blt::busy"];
      [self eval: "namespace import blt::drag&drop"];
      [self eval: "namespace import blt::graph"];
      [self eval: "namespace import blt::BLT_ZoomStack"];
      [self eval: "namespace import blt::vector"];
    }

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

- (const char *)getBltFullVersion
{
  const char *version = [self getBltVersion];

  if (strcmp (version, "8.0") == 0)
    {
      const char *full_version =
        Tcl_GetVar (interp, 
                    "blt::blt_versions(BLT_patchlevel)",
                    TCL_GLOBAL_ONLY);
      if (full_version == NULL)
        return [self getBltVersion];
      else
        return full_version;
    }
  return version;
}

- (BOOL)newBLTp
{
  const char *version_string = [self getBltVersion];
  int major, minor;

  sscanf (version_string, "%d.%d", &major, &minor);
  if (major == 8 && minor == 0)
    {
      const char *full_version_string = [self getBltFullVersion];

      if (full_version_string
          && strcmp (full_version_string, "8.0-unoff") == 0)
        return NO;
      return YES;
    }
  else
    return (major >= 2 && minor >= 3);
}
     
@end
