// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Arguments.h>
#include <misc.h> // strdup
#include <misc/argp.h>
#include <unistd.h> // access
#include <stdlib.h> // getenv

#ifndef __GLIBC__
const char *program_invocation_name;
const char *program_invocation_short_name;
#endif

#include "version.h"

const char *swarm_version = VERSION;

id arguments;

@implementation Arguments

const char *argp_program_version = "Swarm " VERSION;
const char *argp_program_bug_address = "bug-swarm@santafe.edu";

static struct argp_option options[] = {
  {"varyseed", 's', 0, 0, "Run with a random seed", 0},
  {"batch", 'b', 0, 0, "Run in batch mode", 1},
  {"mode", 'm', "MODE", 0, "Specify mode of use (for archiving)", 2},
  { 0 },
};

static const char *
getApplicationValue (const char *val)
{
  const char *appStr;
  {
    const char *ptr;
    
    appStr = val;
    ptr = (char *)appStr;
    while (*ptr)
      {
        if (*ptr == '/')
          appStr = ptr + 1;
        ptr++;
      }
  }
  {
    char *newStr = strdup (appStr);
    char *ptr = newStr;
    
    ptr = newStr;
    while (*ptr)
      {
        if (*ptr == '.')
          {
            *ptr = '\0';
            break;
          }
        ptr++;
      }
    return (const char *)newStr;
  }
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

- (const char *)getExecutablePath
{
  return program_invocation_name;
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
  program_invocation_name = find_executable (theArgv[0]);
#ifndef __GLIBC__
  program_invocation_short_name = getApplicationValue (theArgv[0]);
#endif  
  [arguments setAppName: program_invocation_short_name];
  [arguments setAppModeString: "default"];
  argp_parse (&argp, theArgc, theArgv, 0, 0, arguments);

  return [arguments createEnd];
}

static char *
dropDirectory (char *path)
{
  int start_len = strlen (path), len;

  if (path[start_len - 1] == '/')
    start_len--;

  for (len = start_len; len > 0; len--)
    {
      char *ptr = &path[len - 1];

      if (*ptr == '/' && start_len > 1)
        {
          ptr++;
          *ptr = '\0';
          return path;
        }
    }
  return NULL;
}

static char *
findDirectory (id arguments, const char *directoryName)
{
  char *pathBuf = strdup ([arguments getExecutablePath]);

  while (dropDirectory (pathBuf))
    {
      char *swarmPathBuf = xmalloc (strlen (pathBuf) + strlen (directoryName) + 1);
      
      stpcpy (stpcpy (swarmPathBuf, pathBuf), directoryName);
      if (access (swarmPathBuf, F_OK) != -1)
        return swarmPathBuf;
      xfree (swarmPathBuf);
    }
  return NULL;
}

static char *
findSwarm (id arguments)
{
  const char *swarmPrefix = "swarm-";
  const char *signatureFile = "VERSION";
  int len = strlen (swarmPrefix) + strlen (swarm_version) + 1 + strlen (signatureFile) + 1;
  char *swarmVersionPathBuf = xmalloc (len);
  char *p, *swarmPath;
  
  p = stpcpy (swarmVersionPathBuf, swarmPrefix);
  p = stpcpy (p, swarm_version);
  p = stpcpy (p, "/");
  p = stpcpy (p, signatureFile);
  
  swarmPath = findDirectory (arguments, swarmVersionPathBuf);
  if (swarmPath == NULL)
    swarmPath = findDirectory (arguments, "swarm/VERSION");
  return dropDirectory (swarmPath);
}

- (const char *)getSwarmHome
{
  if (swarmHome == NULL)
    if ((swarmHome = getenv ("SWARMHOME")) == NULL)
      swarmHome = findSwarm (self);
  return swarmHome;
}

@end

