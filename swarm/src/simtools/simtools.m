// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/probing.h>
#import <activity.h>

#import <simtools.h>
#import <simtoolsgui.h> // initSimtoolsGUI

#import <defobj.h> // Arguments

#import <random.h>

#import "InFile.h" // CannotOpenInFile
#import "ObjectLoader.h" // CouldNotInitializeObjectLoader
#import "ObjectSaver.h" // CouldNotSave
#import "OutFile.h" // CannotOpenOutFile
#import "UName.h" // NoBaseNameForUName

BOOL swarmGUIMode;

static void
initSwarmFinish (void)
{
  initProbing ();

  swarmGUIMode = ![arguments getBatchModeFlag];

  initRandom (arguments);

  if (swarmGUIMode)
    initSimtoolsGUI ();
  
  defwarning (CannotOpenOutFile, NULL);
  defwarning (CannotOpenInFile, NULL);
  deferror (UNameError, NULL);
  deferror (CouldNotInitializeObjectLoader, NULL);
  deferror (CouldNotSave, NULL);
}

void
initSwarm (int argc, const char **argv)
{
  initModule (activity);
  initDefobj (argc, argv);
  initSwarmFinish ();
}

void
initSwarmApp (int argc, const char **argv,
              const char *version, const char *bugAddress)
{
  initModule (activity);
  initDefobjApp (argc, argv, version, bugAddress);
  initSwarmFinish ();
}

void
initSwarmAppFunc (int argc, const char **argv,
                  const char *version, const char *bugAddress,
                  struct argp_option *options,
                  int (*parseKeyFunc) (int key, const char *arg))
{
  initModule (activity);
  initDefobjAppFunc (argc, argv, version, bugAddress, options, parseKeyFunc);
  initSwarmFinish ();
}


void
initSwarmArguments (int argc, const char **argv, Class argumentsClass)
{
  initModule (activity);
  initDefobjAppArguments (argc, argv, NULL, NULL, argumentsClass);
  initSwarmFinish ();
}

void
initSwarmAppArguments (int argc, const char **argv,
                       const char *version, const char *bugAddress,
                       Class argumentsClass)
{
  initModule (activity);
  initDefobjAppArguments (argc, argv, version, bugAddress, argumentsClass);
  initSwarmFinish ();
}
