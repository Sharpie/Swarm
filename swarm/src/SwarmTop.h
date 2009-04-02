// Swarm library. Copyright � 1996-2000 Swarm Development Group.
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

#import <Swarm/defobj.h>
#import <Swarm/random.h>
#import <Swarm/objectbase.h>
#ifndef DISABLE_GUI
#ifndef GNUSTEP
#import <Swarm/simtoolsgui.h>
#endif
#endif
#import <Swarm/externvar.h>

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
- (void)verboseMessage: (const char *)message;
- (void)updateDisplay;
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

#ifndef DISABLE_GUI
#ifndef GNUSTEP
- (id <Symbol>)getControlStateRunning;
- (id <Symbol>)getControlStateStopped;
- (id <Symbol>)getControlStateStepping;
- (id <Symbol>)getControlStateQuit;
- (id <Symbol>)getControlStateNextTime;
#endif
#endif

- (id <Zone>)getScratchZone;
- (id <Zone>)getGlobalZone;

- (id <MT19937gen>)getRandomGenerator;
- (id <UniformIntegerDist>)getUniformIntRand;
- (id <UniformDoubleDist>)getUniformDblRand;

#ifndef DISABLE_GUI
#ifndef GNUSTEP
- (id <ProbeLibrary>)getProbeLibrary;
- (id <ProbeDisplayManager>)getProbeDisplayManager;
#endif
#endif

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

#ifdef __cplusplus
extern "C" {
#endif
extern void _initSwarm_ (int argc, const char **argv, const char *appName,
                         const char *version, const char *bugAddress,
                         Class argumentsClass,
                         struct argp_option *options,
                         int (*optionFunc) (int key, const char *arg),
                         BOOL forceBatchMode,
                         BOOL inhibitExecutableSearchFlag);
#ifdef __cplusplus
}
#endif

