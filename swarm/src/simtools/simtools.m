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

static void
run_constructors (void)
{
  extern void libobjc_sl_0_constructor (void);
  extern void libmisc_sl_2_constructor (void);
  extern void libdefobj_sl_2_constructor (void);
  extern void libcollections_sl_2_constructor (void);
  extern void libactivity_sl_3_constructor (void);
  extern void libobjectbase_sl_3_constructor (void);
  extern void librandom_sl_1_constructor (void);
  extern void libtclobjc_sl_1_constructor (void);
  extern void libtkobjc_sl_4_constructor (void);
  extern void libsimtools_sl_3_constructor (void);
  extern void libsimtoolsgui_sl_4_constructor (void);
  extern void libanalysis_sl_3_constructor (void);
  extern void libspace_sl_2_constructor (void);
  extern void libswarm_sl_0_constructor (void);

  printf ("running constructors\n");
  libobjc_sl_0_constructor ();
  libmisc_sl_2_constructor ();
  libdefobj_sl_2_constructor ();
  libcollections_sl_2_constructor ();
  libactivity_sl_3_constructor ();
  libobjectbase_sl_3_constructor ();
  librandom_sl_1_constructor ();
  libtclobjc_sl_1_constructor ();
  libtkobjc_sl_4_constructor ();
  libsimtools_sl_3_constructor ();
  libsimtoolsgui_sl_4_constructor ();
  libanalysis_sl_3_constructor ();
  libspace_sl_2_constructor ();
  libswarm_sl_0_constructor ();
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

