// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/probing.h>
#import <activity.h>

#import <simtools.h>
#import <simtoolsgui.h> // initSimtoolsGUI

#import <simtools/Archiver.h>
#import <objectbase/Arguments.h>

#import <random.h>

#import "InFile.h" // CannotOpenInFile
#import "ObjectLoader.h" // CouldNotInitializeObjectLoader
#import "ObjectSaver.h" // CouldNotSave
#import "OutFile.h" // CannotOpenOutFile
#import "UName.h" // NoBaseNameForUName

int swarmGUIMode;

//M: The initSwarm method initializes the Swarm libraries.  The call to 
//M: initSwarm should be the call in any Swarm code you write.  The argc and
//M: argv are the input parameters to main().
void
initSwarm (int argc, const char **argv)
{
  initSwarmArguments (argc, argv, NULL);
}

void
initSwarmArguments (int argc, const char **argv, Class argumentsClass)
{
  swarmGUIMode = 1;

  initModule (activity);
  initProbing ();

  if (argumentsClass)
    arguments = [argumentsClass createArgc: argc Argv: argv];
  else
    arguments = [Arguments createArgc: argc Argv: argv];
  
  if ([arguments getBatchModeFlag])
    swarmGUIMode = 0;

  archiver = [Archiver ensure: globalZone];

  initRandom (arguments);

  if (swarmGUIMode)
    initSimtoolsGUI ();
  
  defwarning (CannotOpenOutFile, NULL);
  defwarning (CannotOpenInFile, NULL);
  deferror (NoBaseNameForUName, NULL);
  deferror (CouldNotInitializeObjectLoader, NULL);
  deferror (CouldNotSave, NULL);
}

