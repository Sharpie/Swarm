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

#include <stdlib.h>

int swarmGUIMode;
id arguments;

void
initSwarm (int argc, const char **argv)
{
  swarmGUIMode = 1;

  initModule (activity);
  initProbing ();

  arguments = [Arguments createArgc: argc Argv: argv];
  if ([arguments getBatchModeFlag])
    swarmGUIMode = 0;

  archiver = [Archiver ensure: globalZone];

  initRandom (arguments);

  if (swarmGUIMode)
    initSimtoolsGUI ();
  
  // various states used in ControlPanel.
  defsymbol (ControlStateRunning);
  defsymbol (ControlStateStopped);
  defsymbol (ControlStateStepping);
  defsymbol (ControlStateQuit);
  defsymbol (ControlStateNextTime);
}

