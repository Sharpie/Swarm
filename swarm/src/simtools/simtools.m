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
init (int argc, const char **argv, 
      const char *version, const char *bugAddress,
      Class argumentsClass)
{
  initModule (activity);

  initDefobj (argc, argv, version, bugAddress,
              options, optionFunc,
              argumentsClass);

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
  init (argc, argv, NULL, NULL, NULL, NULL, [Arguments class]);
}

void
initSwarmApp (int argc, const char **argv,
              const char *version, const char *bugAddress)
{
  init (argc, argv, version, bugAddress, NULL, NULL, [Arguments class]);
}

void
initSwarmAppOptions (int argc, const char **argv,
                     const char *version, const char *bugAddress,
                     struct argp_option *options,
                     int (*optionFunc) (int key, const char *arg))
{
  init (argc, argv,
        version, bugAddress,
        options, optionFunc,
        [Arguments class]);
}


void
initSwarmArguments (int argc, const char **argv, Class argumentsClass)
{
  init (argc, argv, NULL, NULL, NULL, NULL, argumentsClass);
}

void
initSwarmAppArguments (int argc, const char **argv,
                       const char *version, const char *bugAddress,
                       Class argumentsClass)
{
  init (argc, argv, version, bugAddress, NULL, NULL, argumentsClass);
}

