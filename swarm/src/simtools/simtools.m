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

BOOL swarmGUIMode = NO;

static void
init (int argc, const char **argv, 
      const char *version, const char *bugAddress,
      Class argumentsClass,
      struct argp_option *options,
      int (*optionFunc) (int key, const char *arg),
      BOOL forceBatchMode)
{
  initModule (activity);

  initDefobj (argc, argv,
              version, bugAddress,
              argumentsClass,
              options, optionFunc);

  initProbing ();

  if (![arguments getBatchModeFlag] && !forceBatchMode)
    swarmGUIMode = YES;
  
  initRandom (arguments);
  
  if (swarmGUIMode)
    initSimtoolsGUI ();
}

void
initSwarm (int argc, const char **argv)
{
  init (argc, argv, NULL, NULL, Nil, NULL, NULL, NO);
}

void
initSwarmBatch (int argc, const char **argv)
{
  init (argc, argv, NULL, NULL, Nil, NULL, NULL, YES);
}

void
initSwarmApp (int argc, const char **argv,
              const char *version, const char *bugAddress)
{
  init (argc, argv, version, bugAddress, Nil, NULL, NULL, NO);
}

void
initSwarmAppBatch (int argc, const char **argv,
              const char *version, const char *bugAddress)
{
  init (argc, argv, version, bugAddress, Nil, NULL, NULL, YES);
}

void
initSwarmAppOptions (int argc, const char **argv,
                     const char *version, const char *bugAddress,
                     struct argp_option *options,
                     int (*optionFunc) (int key, const char *arg))
{
  init (argc, argv,
        version, bugAddress,
        Nil,
        options, optionFunc,
        NO);
}

void
initSwarmAppOptionsBatch (int argc, const char **argv,
                     const char *version, const char *bugAddress,
                     struct argp_option *options,
                     int (*optionFunc) (int key, const char *arg))
{
  init (argc, argv,
        version, bugAddress,
        Nil,
        options, optionFunc,
        YES);
}

void
initSwarmArguments (int argc, const char **argv, Class argumentsClass)
{
  init (argc, argv, NULL, NULL, argumentsClass, NULL, NULL, NO);
}

void
initSwarmAppArguments (int argc, const char **argv,
                       const char *version, const char *bugAddress,
                       Class argumentsClass)
{
  init (argc, argv, version, bugAddress, argumentsClass, NULL, NULL, NO);
}
