// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Arguments.h>
#include <misc/argp.h>

#ifndef __GLIBC__
const char *program_invocation_name;
const char *program_invocation_short_name;
#endif

@implementation Arguments

const char *argp_program_version = "Swarm 1.0.5";
const char *argp_program_bug_address = "bug-swarm@santafe.edu";

static struct argp_option options[] = {
  {"varyseed", 's', 0, 0, "Run with a random seed", 0},
  {"batch", 'b', 0, 0, "Run in batch mode", 1},
  {"mode", 'm', "MODE", 0, "Specify mode of use (for archiving)", 2},
  { 0 },
};

static const char *
getApplicationValue (const char *ptr)
{
  const char *appStr = ptr;
  
  while (*ptr)
    {
      if (*ptr == '/')
        appStr = ptr + 1;
      ptr++;
    }
  return appStr;
}

static error_t
parse_opt (int key, const char *arg, struct argp_state *state)
{
  id arguments = state->input;

  switch (key)
    {
    case 's':
      [arguments setVarySeedFlag: YES];
      break;
    case 'b':
      [arguments setBatchModeFlag: YES];
      break;
    case 'a':
      [arguments setAppModeString: getApplicationValue (arg)];
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

static struct argp argp = { options, parse_opt, NULL, NULL };

- setArgc: (int)theArgc Argv: (const char **)theArgv
{
  argc = theArgc;
  argv = theArgv;
  return self;
}

- setAppName: (const char *)theApplicationName;
{
  applicationName = theApplicationName;
  return self;
}

- setAppModeString: (const char *)theAppModeString
{
  appModeString = theAppModeString;
  return self;
}

- setBatchModeFlag: (BOOL)theBatchModeFlag
{
  batchModeFlag = theBatchModeFlag;
  return self;
}

- setVarySeedFlag: (BOOL)theVarySeedFlag
{
  varySeedFlag = theVarySeedFlag;
  return self;
}

- (BOOL)getBatchModeFlag
{
  return batchModeFlag;
}

- (BOOL)getVarySeedFlag
{
  return varySeedFlag;
}

- (const char *)getAppName
{
  return applicationName;
}

- (const char *)getAppModeString
{
  return appModeString;
}

- (int)getArgc
{
  return argc;
}

- (const char **)getArgv
{
  return argv;
}

+ createArgc: (int)theArgc Argv: (const char **)theArgv
{
  id arguments = [Arguments createBegin: globalZone];
  
  [arguments setArgc: theArgc Argv: theArgv];
#ifndef __GLIBC__
  program_invocation_name = theArgv[0];
  program_invocation_short_name = getApplicationValue (theArgv[0]);
#endif  
  [arguments setAppName: program_invocation_short_name];
  [arguments setAppModeString: "default"];
  argp_parse (&argp, theArgc, theArgv, 0, 0, arguments);
  return [arguments createEnd];
}

@end

