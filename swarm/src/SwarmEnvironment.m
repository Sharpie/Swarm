#import <SwarmEnvironment.h>

#import <simtools.h>
#import <collections.h>
#import <defobj.h>
#import <activity.h>
#import <random.h>
#import <objectbase.h>
#import <simtoolsgui.h>

#import <objectbase/probing.h> // initProbing
#import <defobj/Customize.h>  // PHASE

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

@implementation SwarmEnvironment
PHASE(Creating)
+ createBegin
{
#ifdef hpux
  run_constructors ();
#endif
  initModule (activity);
  return [[self alloc] init];
}

- setArguments: (id <Arguments>)_arguments
{
  arguments = _arguments;
  return self;
}

- setBatchMode: (BOOL)_forceBatchMode
{
  forceBatchMode = _forceBatchMode;
  return self;
}

- createEnd
{
  initDefobj (arguments);
  initProbing ();

  if (![arguments getBatchModeFlag] && !forceBatchMode)
    swarmGUIMode = YES;
  
  initRandom (arguments);
  
  if (swarmGUIMode)
    initSimtoolsGUI ();
  return self;
}
 
#include "SwarmEnvironment_getters.m"
@end
