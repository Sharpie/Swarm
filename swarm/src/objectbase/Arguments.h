// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface Arguments: SwarmObject
{
  int argc;
  const char **argv;
  const char *applicationName;
  const char *appModeString;
  BOOL batchModeFlag;
  BOOL varySeedFlag;
}

- setArgc: (int)theArgc Argv: (const char **)theArgv;
- setAppName: (const char *)appName;
- setAppModeString: (const char *)appModeString;
- setBatchModeFlag: (BOOL)batchModeFlag;
- setVarySeedFlag: (BOOL)varySeedFlag;
- (BOOL)getBatchModeFlag;
- (BOOL)getVarySeedFlag;
- (const char *)getAppName;
- (const char *)getAppModeString;
- (int)getArgc;
- (const char **)getArgv;
+ createArgc: (int)argc Argv: (const char **)argv;

@end

