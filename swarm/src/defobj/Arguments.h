// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>

#include <swarmconfig.h>
#ifdef HAVE_ARGP_H
#include <argp.h>
#else
#include <misc/argp.h>
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
  BOOL showCurrentTimeFlag;
  const char *swarmHome;
  const char *defaultAppConfigPath;
  const char *defaultAppDataPath;
  BOOL ignoringEnvFlag;
  int (*optionFunc) (int key, const char *arg);
  int lastArgIndex;
  struct argp *argp;
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
- setDefaultAppConfigPath: (const char *)path;
- setDefaultAppDataPath: (const char *)path;
- setInhibitArchiverLoadFlag: (BOOL)inhibitArchiverLoadFlag;
- setInhibitExecutableSearchFlag: (BOOL)theInhibitExecutableSearchFlag;

- (BOOL)getBatchModeFlag;
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

