#import <defobj.h>
#import <random.h>
#import <objectbase.h>
#import <simtoolsgui.h>
#import <externvar.h>

//S: Top-level Swarm module

//D: Top-level module for controlling startup and providing access to globals

@protocol SwarmEnvironment <CREATABLE>
//S: Container object for Swarm globals

//D: Container object for Swarm globals
CREATING
+ createBegin; // bootstrapping -- there are no Zones available yet
- setArguments: (id <Arguments>)arguments;
- setBatchMode: (BOOL)batchMode;
- createEnd;
+ initSwarm: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress argCount: (unsigned)count args: (const char **)args;
USING
- (void)initSwarmUsing: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress args: (const char **)args;
- (timeval_t)getCurrentTime;
- (id <SwarmActivity>)getCurrentSwarmActivity;
- (void)createProbeDisplay: obj;
- (void)createCompleteProbeDisplay: obj;
- (void)createArchivedProbeDisplay: obj name: (const char *)name;
- (void)createArchivedCompleteProbeDisplay: obj name: (const char *)name;
- (void)setWindowGeometryRecordName: obj name: (const char *)name;
- (void)setComponentWindowGeometryRecordNameFor: obj widget: widget name: name;
- (void)setComponentWindowGeometryRecordName: widget name: name;
- (void)xprint: obj;
- (void)xfprint: obj;
- (void)dumpDirectory;
- (const char *)typeModule: (const char *)typeName;
GETTERS
- (id <Arguments>)getArguments;
- (id <Symbol>)getStart;
- (id <Symbol>)getMember;
- (id <Symbol>)getEnd;

- (id <Symbol>)getInitialized;
- (id <Symbol>)getRunning;
- (id <Symbol>)getStopped;
- (id <Symbol>)getHolding;
- (id <Symbol>)getReleased;
- (id <Symbol>)getTerminated;
- (id <Symbol>)getCompleted;

- (id <Symbol>)getRandomized;
- (id <Symbol>)getSequential;

- (id <Symbol>)getControlStateRunning;
- (id <Symbol>)getControlStateStopped;
- (id <Symbol>)getControlStateStepping;
- (id <Symbol>)getControlStateQuit;
- (id <Symbol>)getControlStateNextTime;

- (id <Zone>)getScratchZone;
- (id <Zone>)getGlobalZone;

- (id <MT19937gen>)getRandomGenerator;
- (id <UniformIntegerDist>)getUniformIntRand;
- (id <UniformDoubleDist>)getUniformDblRand;

- (id <ProbeLibrary>)getProbeLibrary;
- (id <ProbeDisplayManager>)getProbeDisplayManager;

- (id <Archiver>)getHdf5Archiver;
- (id <Archiver>)getLispArchiver;
- (id <Archiver>)getHdf5AppArchiver;
- (id <Archiver>)getLispAppArchiver;
- (BOOL)getGuiFlag;

- (id <Symbol>)getLanguageCOM;
- (id <Symbol>)getLanguageJava;
- (id <Symbol>)getLanguageObjc;
@end

//G: Flag for whether we're in graphics mode or not.  Default is NO.
externvar BOOL swarmGUIMode;

@class SwarmEnvironment;

extern void _initSwarm_ (int argc, const char **argv, const char *appName,
                         const char *version, const char *bugAddress,
                         Class argumentsClass,
                         struct argp_option *options,
                         int (*optionFunc) (int key, const char *arg),
                         BOOL forceBatchMode,
                         BOOL inhibitExecutableSearchFlag);

