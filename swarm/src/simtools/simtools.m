// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
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

#ifdef hpux
static void
run_constructors (void)
{
  extern void libobjc_constructor (void);
  extern void libmisc_constructor (void);
  extern void libdefobj_constructor (void);
  extern void libcollections_constructor (void);
  extern void libactivity_constructor (void);
  extern void libobjectbase_constructor (void);
  extern void librandom_constructor (void);
  extern void libtclobjc_constructor (void);
  extern void libtkobjc_constructor (void);
  extern void libsimtools_constructor (void);
  extern void libsimtoolsgui_constructor (void);
  extern void libanalysis_constructor (void);
  extern void libspace_constructor (void);
  extern void libswarm_constructor (void);

  libobjc_constructor ();
  libmisc_constructor ();
  libdefobj_constructor ();
  libcollections_constructor ();
  libactivity_constructor ();
  libobjectbase_constructor ();
  librandom_constructor ();
  libtclobjc_constructor ();
  libtkobjc_constructor ();
  libsimtools_constructor ();
  libsimtoolsgui_constructor ();
  libanalysis_constructor ();
  libspace_constructor ();
  libswarm_constructor ();
}
#endif 

void
_initSwarm_ (int argc, const char **argv, const char *appName,
      const char *version, const char *bugAddress,
      Class argumentsClass,
      struct argp_option *options,
      int (*optionFunc) (int key, const char *arg),
      BOOL forceBatchMode)
{
#ifdef hpux
  run_constructors ();
#endif
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

