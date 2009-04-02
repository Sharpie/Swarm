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

#import <Swarm/Create.h>

#import <Swarm/swarmconfig.h>
#ifdef HAVE_ARGP_H
#include <argp.h>
#else
#include <Swarm/argp.h>
#endif

@interface Arguments_c: CreateDrop
{
  int argc;
  const char **argv;
  const char *applicationName;
  const char *executablePath;
  const char *appModeString;
  const char *version;
  const char *bugAddress;
  BOOL inhibitArchiverLoadFlag;
  BOOL inhibitExecutableSearchFlag;
  BOOL batchModeFlag;
  BOOL varySeedFlag;
  BOOL fixedSeedFlag;
  unsigned fixedSeed;
  BOOL verboseFlag;
  BOOL showCurrentTimeFlag;
  const char *swarmHome;
  const char *defaultAppConfigPath;
  const char *defaultAppDataPath;
  BOOL ignoringEnvFlag;
  int (*optionFunc) (int key, const char *arg);
  int lastArgIndex;
  void *argp;
}
+ createBegin: aZone;
- createEnd;
+ createArgc: (int)argc Argv: (const char **)argv appName: (const char *)appName version: (const char *)version bugAddress: (const char *)bugAddress options: (struct argp_option *)options optionFunc: (int (*) (int, const char *))optionFunc inhibitExecutableSearchFlag: (BOOL)inhibitExecutableSearchFlag;
- (void)addOptions: (struct argp_option *)options;
- (void)addOption: (const char *)name key: (int)key arg: (const char *)arg flags: (int)flags doc: (const char *)doc group: (int)group;

- (int)parseKey: (int)key arg: (const char *)arg;

- setArgc: (unsigned)theArgc Argv: (const char **)theArgv;
- setAppName: (const char *)appName;
- setAppModeString: (const char *)appModeString;
- setVersion: (const char *)version;
- setBugAddress: (const char *)bugAddress;
- setOptionFunc: (int (*) (int key, const char *arg))optionFunc;
- setBatchModeFlag: (BOOL)batchModeFlag;
- setShowCurrentTimeFlag: (BOOL)timeFlag;
- setVarySeedFlag: (BOOL)varySeedFlag;
- setVerboseFlag: (BOOL)verboseFlag;
- setDefaultAppConfigPath: (const char *)path;
- setDefaultAppDataPath: (const char *)path;
- setInhibitArchiverLoadFlag: (BOOL)inhibitArchiverLoadFlag;
- setInhibitExecutableSearchFlag: (BOOL)theInhibitExecutableSearchFlag;
- setFixedSeed: (unsigned)seed;

- (BOOL)getVerboseFlag;
- (BOOL)getBatchModeFlag;
- (BOOL)getFixedSeedFlag;
- (unsigned)getFixedSeed;
- (BOOL)getVarySeedFlag;
- (BOOL)getShowCurrentTimeFlag;
- (BOOL)getInhibitArchiverLoadFlag;
- (const char *)getAppName;
- (const char *)getAppModeString;
- (int)getArgc;
- (int)getLastArgIndex;
- (const char **)getArgv;
- (const char *)getExecutablePath;
- (const char *)getSwarmHome;
- (const char *)getConfigPath;
- (const char *)getDataPath;
- (const char *)getAppConfigPath;
- (const char *)getAppDataPath;
@end
