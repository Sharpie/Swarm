// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <SwarmEnvironment.h>

#import <collections.h>
#import <defobj.h>
#import <activity.h>
#import <random.h>
#import <objectbase.h>
#import <simtools.h>
#ifndef DISABLE_GUI
#ifndef GNUSTEP
#import <simtoolsgui.h>
#endif
#endif

#ifdef ENABLE_XMLRPC
#import <swarm_xmlrpc.h>
#endif

#import <objectbase/probing.h> // initProbing
#import <defobj/Customize.h>  // PHASE
#import <defobj/Arguments.h> // Arguments_c

#ifndef DISABLE_GUI
#if !defined(GNUSTEP) && !defined(SWARM_OSX)
#import <gui.h> // GUI_EVENT_ASYNC
#endif
#endif

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

struct Zone_c;
struct ComponentZone_c;
struct Object_s;

id (*_swarm_i_Zone_c__allocIVarsComponent_) (struct Zone_c *, SEL, Class);
void (*_swarm_i_Zone_c__freeIVarsComponent_) (struct Zone_c *, SEL, id);
void * (*_swarm_i_Zone_c__allocBlock_) (struct Zone_c *, SEL, size_t);
void (*_swarm_i_Zone_c__freeBlock_blockSize_) (struct Zone_c *, SEL, void *, size_t);
id (*_swarm_i_ComponentZone_c__allocIVars_) (struct ComponentZone_c *, SEL, Class);
id (*_swarm_i_Object_s__drop) (struct Object_s *, SEL);   

// static void predispatch () __attribute__ ((constructor));

static void predispatch ()
{
  _swarm_i_Zone_c__allocIVarsComponent_ = (void *)swarm_class_getMethodImplementation (swarm_objc_lookupClass ("Zone_c"), M(allocIVarsComponent:));
  _swarm_i_Zone_c__freeIVarsComponent_ = (void *)swarm_class_getMethodImplementation (swarm_objc_lookupClass ("Zone_c"), M(freeIVarsComponent:));
  _swarm_i_Zone_c__allocBlock_ = (void *)swarm_class_getMethodImplementation (swarm_objc_lookupClass ("Zone_c"), M(allocBlock:));
  _swarm_i_Zone_c__freeBlock_blockSize_ = (void *)swarm_class_getMethodImplementation (swarm_objc_lookupClass ("Zone_c"), M(freeBlock:blockSize:));
  _swarm_i_ComponentZone_c__allocIVars_ = (void *)swarm_class_getMethodImplementation (swarm_objc_lookupClass ("ComponentZone_c"), M(allocIVars:));
  _swarm_i_Object_s__drop = (void *)swarm_class_getMethodImplementation (swarm_objc_lookupClass ("Object_s"), M(drop));
}


@implementation SwarmEnvironment
PHASE(Creating)
+ createBegin
{
#ifdef hpux
  run_constructors ();
#endif
  predispatch ();
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

#ifndef DISABLE_GUI  
  if (![arguments getBatchModeFlag] && !forceBatchMode)
    swarmGUIMode = YES;
#else
  swarmGUIMode = NO;
#endif

  initRandom (arguments);

#ifndef DISABLE_GUI  
#if !defined(GNUSTEP) && !defined(SWARM_OSX)
  if (swarmGUIMode)
    initSimtoolsGUI ();
#endif
#endif

#ifdef HAVE_JDK
  {
    jobject jself = SD_JAVA_FIND_OBJECT_JAVA (self);
    
    if (jself)
      {
        jobject nextPhase = SD_JAVA_NEXTPHASE (jself);
        
        swarm_directory_java_associate_objects (nextPhase);
        (*jniEnv)->DeleteLocalRef (jniEnv, nextPhase);
      }
  }
#endif
 
#ifdef ENABLE_XMLRPC 
  initSwarmXMLRPC();
#endif
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

// It is declared in Using phase (so that it shows up in Globals.env),
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

#ifndef DISABLE_GUI
#if !defined(GNUSTEP) && !defined(SWARM_OSX)
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

- (void)updateDisplay
{
  while (GUI_EVENT_ASYNC ()) {}
}
#endif // GNUSTEP, SWARM_OSX
#endif // DISABLE_GUI

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
  id env;

#if !defined(GNUSTEP) && !defined(SWARM_OSX)
  void __objc_exec_class_for_all_initial_modules ();

  __objc_exec_class_for_all_initial_modules ();
#endif

  env = [SwarmEnvironment createBegin];

#if DEBUG
  fprintf(stderr, "Swarm Environment\n");
  fprintf(stderr, "argc argv[0]: %d %s\n", argc, argv[0]);
#endif
  [env setArguments:
         [(id) argumentsClass ?: (id) [Arguments_c class]
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
