// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/probing.h>
#import <activity.h>

#import <simtools.h>
#import <simtoolsgui.h> // initSimtoolsGUI

#import <defobj.h> // Arguments

#import <random.h>

externvardef BOOL swarmGUIMode = NO;

void
_initSwarm_ (int argc, const char **argv, const char *appName,
      const char *version, const char *bugAddress,
      Class argumentsClass,
      struct argp_option *options,
      int (*optionFunc) (int key, const char *arg),
      BOOL forceBatchMode)
{
  initModule (activity);

  initDefobj (argc, argv, 
              appName, version, bugAddress,
              argumentsClass,
              options, optionFunc);

  initProbing ();

  if (![arguments getBatchModeFlag] && !forceBatchMode)
    swarmGUIMode = YES;
  
  initRandom (arguments);
  
  if (swarmGUIMode)
    initSimtoolsGUI ();
}
