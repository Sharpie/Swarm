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

#import <swarm.h>

#import <defobj.h>
#import <collections.h>
#import <objectbase.h>
#import <random.h>
#import <simtools.h>
#import <simtoolsgui.h>
#import <defobj/Create.h>

@interface SwarmEnvironment: CreateDrop <SwarmEnvironment>
{
  id <Arguments> arguments;
  BOOL forceBatchMode;
}
+ createBegin;
- setArguments: (id <Arguments>)arguments;
- setBatchMode: (BOOL)batchMode;
- createEnd;
- _init_: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress argCount: (unsigned)count args: (const char **)args;
+ initSwarm: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress argCount: (unsigned)count args: (const char **)args;
- (void)initSwarmUsing: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress args: (const char **)args;

- (timeval_t)getCurrentTime;
- (id <SwarmActivity>)getCurrentSwarmActivity;

- (void)createProbeDisplay: obj;
- (void)createCompleteProbeDisplay: obj;
- (void)createArchivedProbeDisplay: obj name: (const char *)name;
- (void)createArchivedCompleteProbeDisplay: obj name: (const char *)name;
- (void)setWindowGeometryRecordName: obj name: (const char *)name;
- (void)setComponentWindowGeometryRecordNameFor: obj widget: widget name: (const char *)name;
- (void)setComponentWindowGeometryRecordName: widget name: (const char *)name;
- (void)xprint: obj;
- (void)xfprint: obj;
- (void)dumpDirectory;
- (const char *)typeModule: (const char *)typeName;
- (void)verboseMessage: (const char *)message;
- (void)updateDisplay;

#import "SwarmEnvironment_getters.h"
@end
