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
GETTERS
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
- (id <Symbol>)getControlStateQuit;
- (id <Symbol>)getControlStateNextTime;

- (id <Zone>)getScratchZone;
- (id <Zone>)getGlobalZone;

- (id <SimpleRandomGenerator>)getRandomGenerator;
- (id <UniformIntegerDist>)getUniformIntRand;
- (id <UniformDoubleDist>)getUniformDblRand;

- (id <ProbeLibrary>)getProbeLibrary;
- (id <ProbeDisplayManager>)getProbeDisplayManager;

- (id <Archiver>)getHdf5Archiver;
- (id <Archiver>)getLispArchiver;
- (id <Archiver>)getHdf5AppArchiver;
- (id <Archiver>)getLispAppArchiver;
- (BOOL)getGuiFlag;
@end

//G: Flag for whether we're in graphics mode or not.  Default is NO.
externvar BOOL swarmGUIMode;

