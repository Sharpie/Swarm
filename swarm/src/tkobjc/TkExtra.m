// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/TkExtra.h>
#import <defobj.h> // OSTRDUP

#include <tcl.h>
#include <misc.h> // access

@implementation TkExtra

int Blt_Init(Tcl_Interp *);			  // wish this were declared..

static void
ensureBltSupportFiles (id arguments, id globalTkInterp)
{
  const char *fileName = "bltGraph.tcl";
  const char *basePath = [globalTkInterp globalVariableValue: "blt_library"];
  BOOL retry = NO;
  
  do { 
    char buf[strlen (basePath) + 1 + strlen (fileName) + 1];
    char *p;
    
    p = stpcpy (buf, basePath);
    p = stpcpy (p, "/");
    p = stpcpy (p, fileName);
    
    if (access (buf, F_OK) == -1)
      {
        if (!retry)
          {
            const char *swarmHome = [arguments getSwarmHome];

            if (swarmHome)
              {
                const char *libdir = "share/blt2.4";
                char libPath[strlen (swarmHome) + strlen (libdir) + 1];
                char *p;
                
                p = stpcpy (libPath, swarmHome);
                stpcpy (p, libdir);
                
                basePath = OSTRDUP (arguments, libPath); 
                [globalTkInterp globalEval: "set blt_library \"%s\"",
				libPath];
              }
            else
              {
                basePath = ".";
                [globalTkInterp globalEval: "set blt_library \"%s\"",
				basePath];
              }
            retry = YES;
            continue;
          }
        else
          {
            fprintf (stderr, "BLT support file `%s' not found\n", fileName);
            fprintf (stderr, "If the directory `%s' not the intended location for "
                     "the BLT support files,\nplease adjust BLT_LIBRARY to the "
                     "right place.\n", basePath);
            exit (1);
          }
      }
    else
      break;
  } while (1);
}

- (const char *)preInitWithArgc: (int)argc argv: (const char **)argv
{
  const char *filename = [super preInitWithArgc: argc argv: argv];

  // now init extras widget sets.

  if (Blt_Init (interp) != TCL_OK)
    {
      char *msg = Tcl_GetVar (interp, "errorInfo", TCL_GLOBAL_ONLY);
      
      if (msg == NULL)
        msg = interp->result;
      [self error:msg];
      return NULL;				  // shouldn't get here anyway
    }

  ensureBltSupportFiles (arguments, self);

  // This avoids the need for consultation of tclIndex, which is useful
  // when distributing just the minimum set of Tcl support files.
  [self eval: "source $blt_library/bltGraph.tcl"];

  [self eval: "wm withdraw ."];			  // don't map "."
  
  // general problem: better mechanism for assigning bindings.
  // note, the "%s" indirection in eval: is necessary to hide %s from eval:
  // Death to Motif lossage! Delete should delete the character to the left.
  // My precedent is emacs and xterm behaviour, indeed every bit of code that
  // Isn't Motif. If you want to delete forward, Control-d still works.
  [self eval: "%s", 
        "bind Entry <Delete> [bind Entry <BackSpace>]; "
        "bind Text <Delete> [bind Text <BackSpace>]"];

  {
    const char *fullVersion = [self getBltFullVersion];

    if (strcmp (fullVersion, "8.0-unoff") == 0
        || strcmp (fullVersion, "2.4") == 0)
      {
        [self eval: "namespace import blt::barchart"];
        [self eval: "namespace import blt::bitmap"];
        [self eval: "namespace import blt::busy"];
        [self eval: "namespace import blt::drag&drop"];
        [self eval: "namespace import blt::graph"];
        [self eval: "namespace import blt::BLT_ZoomStack"];
        [self eval: "namespace import blt::vector"];
        [self eval: "namespace import blt::table"];
        // Without the load below, on VarProbeEntry double-clicks,
        // this occured:
        //   Original error: no value given for parameter "start" to
        //   "tcl_wordBreakBefore"
        // -mgd
        [self eval: "if {[info library] == \"\"} { "
              "source ./word.tcl "
              "} else { "
              "source [info library]/word.tcl "
              "}"];
      }
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
      const char *var = "blt::blt_versions(BLT_patchlevel)";
      char buf[strlen(var) + 1];

      strcpy (buf, var);
      {
        const char *full_version = Tcl_GetVar (interp, buf, TCL_GLOBAL_ONLY);

        if (full_version == NULL)
          return [self getBltVersion];
        else
          return full_version;
      }
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
