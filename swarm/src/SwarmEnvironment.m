#import <SwarmEnvironment.h>

#import <collections.h>
#import <defobj.h>
#import <activity.h>
#import <random.h>
#import <objectbase.h>
#import <simtools.h>
#import <simtoolsgui.h>

#import <objectbase/probing.h> // initProbing
#import <defobj/Customize.h>  // PHASE
#import <defobj/Arguments.h> // Arguments_c

#include <swarmconfig.h>

#ifdef HAVE_JDK
#import <defobj/java.h>
#endif

#import <defobj/COM.h>
#import <defobj/directory.h>

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

#ifdef HAVE_JDK
  {
    jobject jself = SD_JAVA_FIND_OBJECT_JAVA (self);
    
    if (jself)
      {
        jobject nextPhase = SD_JAVA_NEXTPHASE (jself);
        
        swarm_directory_java_associate_objects (nextPhase);
        (*jniEnv)->DeleteLocalRef (jniEnv, nextPhase);
      }
#endif
  }
  return self;
}

+ initSwarm: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress argCount: (unsigned)count args: (const char **)args
{
  id obj = [SwarmEnvironment createBegin];

  if (COM_init_p ())
    swarmDirectory = [Directory create: globalZone];
  return [obj _init_: appName version: version bugAddress: bugAddress argCount: count args: args];
}

PHASE(Setting)

- _init_: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress argCount: (unsigned)argc args: (const char **)args
{
  unsigned i;

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
  return [self createEnd];
}

// It is declare in Using phase (so that it shows up in Globals.env),
// but technically will run from the Java stub in Create phase.
- (void)initSwarmUsing: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress args: (const char **)args
{
  unsigned argc = 0;

  while (args[argc]) argc++;

  [self _init_: appName version: version bugAddress: bugAddress argCount: argc args: args];
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

- (void)createProbeDisplay: obj
{
  CREATE_PROBE_DISPLAY (obj);
}

- (void)createCompleteProbeDisplay: obj
{
  CREATE_COMPLETE_PROBE_DISPLAY (obj);
}

- (void)createArchivedProbeDisplay: obj name: (const char *)name
{
  createArchivedProbeDisplayNamed (obj, name);
}

- (void)createArchivedCompleteProbeDisplay: obj name: (const char *)name
{
  createArchivedCompleteProbeDisplayNamed (obj, name);
}

- (void)setWindowGeometryRecordName: obj name: (const char *)name
{
  [obj setWindowGeometryRecordName: name];
}

- (void)setComponentWindowGeometryRecordNameFor: obj 
                                         widget: widget
                                           name: (const char *)name
{
  [obj setWindowGeometryRecordNameForComponent: name widget: widget];
}

- (void)setComponentWindowGeometryRecordName: widget name: (const char *)name
{
  [self setComponentWindowGeometryRecordNameFor: self
        widget: widget
        name: name];
}

- (void)xprint: obj
{
  xprint (obj);
}

- (void)xfprint: obj
{
  xfprint (obj);
}

- (void)dumpDirectory
{
#ifdef HAVE_JDK
  swarm_directory_dump ();
#endif
}

- (const char *)typeModule: (const char *)typeName
{
  extern const char *swarm_lookup_module (const char *typeName);

  return swarm_lookup_module (typeName);
}

- (void)verboseMessage: (const char *)str
{
  if ([arguments getVerboseFlag])
    puts (str);
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
