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
#import <defobj/Arguments.h> // Arguments_c

#include <swarmconfig.h>

#ifdef HAVE_JDK
#import <defobj/java.h>
#endif

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
  return [self createBegin: globalZone];
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

PHASE(Setting)

- (void)initSwarm: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress args: (const char **)args
{
  int argc, i;

  for (argc = 0; args[argc]; argc++)
    {
    }
  {
    const char *argv[argc + 1];
    
    argv[0] = appName;
    
    for (i = 0; i < argc; i++)
      argv[i + 1] = args[i];
        
    [self setArguments: 
            [Arguments_c createArgc: argc + 1
                         Argv: argv
                         appName: appName
                         version: version
                         bugAddress: bugAddress
                         options: NULL
                         optionFunc: NULL
                         inhibitExecutableSearchFlag: YES]];
  }
  [self createEnd];
}

PHASE(Using)

- (timeval_t)getCurrentTime
{
  return getCurrentTime ();
}

- (id <SwarmActivity>)getCurrentSwarmActivity
{
  return getCurrentSwarmActivity ();
}

- (void)createArchivedProbeDisplay: obj name: (const char *)name
{
  createArchivedProbeDisplayNamed (obj, name);
}

- (void)setWindowGeometryRecordName: obj name: (const char *)name
{
  [obj setWindowGeometryRecordName: name];
}


void
_initSwarm_ (int argc, const char **argv, const char *appName,
             const char *version, const char *bugAddress,
             Class argumentsClass,
             struct argp_option *options,
             int (*optionFunc) (int key, const char *arg),
             BOOL forceBatchMode,
             BOOL inhibitExecutableSearchFlag)
{
  id env = [SwarmEnvironment createBegin];

  [env setArguments:
         [argumentsClass ?: [Arguments_c class]
                         createArgc: argc
                         Argv: argv
                         appName: appName
                         version: version
                         bugAddress: bugAddress
                         options: options
                         optionFunc: optionFunc
                         inhibitExecutableSearchFlag:
                           inhibitExecutableSearchFlag]];
  [env setBatchMode: forceBatchMode];

  [env createEnd];
}

#include "SwarmEnvironment_getters.m"
@end
