// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Arguments.h>

#include <misc.h> // strdup, getenv, access
#include <misc/argp.h>

#ifndef __GLIBC__
const char *program_invocation_name;
const char *program_invocation_short_name;
#endif

#define SIGNATURE_FILE "VERSION"

#include "version.h"

const char *swarm_version = SWARM_VERSION;

id arguments;

@implementation Arguments

const char *argp_program_version = "Swarm " SWARM_VERSION;
const char *argp_program_bug_address = "bug-swarm@santafe.edu";

static struct argp_option base_options[] = {
  {"varyseed", 's', 0, 0, "Run with a random seed", 0},
  {"batch", 'b', 0, 0, "Run in batch mode", 1},
  {"mode", 'm', "MODE", 0, "Specify mode of use (for archiving)", 2},
  { 0 }
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

- (int)parseKey: (int)key arg: (const char *)arg
{
  switch (key)
    {
    case 's':
      [self setVarySeedFlag: YES];
      break;
    case 'b':
      [self setBatchModeFlag: YES];
      break;
    case 'a':
      [self setAppModeString: getApplicationValue (arg)];
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

static error_t
parse_opt (int key, const char *arg, struct argp_state *state)
{
  id arguments = state->input;

  return [arguments parseKey: key arg: arg];
}

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

static struct argp *argp;

- (struct argp_option *)addOptions: (struct argp_option *)newoptions
{
  unsigned exist_count = 0, total_count = 0, new_count = 0;
  struct argp_option *options = (struct argp_option *)argp->options;
  
  if (argp->options)
    {
      while (options->name)
        {
          exist_count++;
          total_count++;
          options++;
        }
    }
  options = newoptions;
  while (options->name)
    {
      total_count++;
      new_count++;
      options++;
    }
  if (argp->options)
    options = xrealloc ((void *)argp->options,
                              (total_count + 1) * sizeof (struct argp_option));
  else
    options = xmalloc ((total_count + 1) * sizeof (struct argp_option));
  
  memcpy (&options[exist_count],
          newoptions,
          sizeof (struct argp_option) * new_count);
  {
    struct argp_option *end = &options[total_count];
    
    end->name = NULL;
    end->key = 0;
    end->doc = NULL;
    end->group = 0;
  }
  return options;
}

+ createBegin: aZone
{
  Arguments *obj = [super createBegin: aZone];
  
  argp = xmalloc (sizeof (struct argp));
  argp->options = NULL;
  argp->options = [obj addOptions: base_options];
  argp->parser = parse_opt;
  argp->args_doc = NULL;
  argp->doc = NULL;
  argp->children = NULL;
  argp->help_filter = NULL;

  return obj;
}

+ createArgc: (int)theArgc
        Argv: (const char **)theArgv
{
  id arguments = [self createBegin: globalZone];
  
  [arguments setArgc: theArgc Argv: theArgv];
  program_invocation_name = (char *)find_executable (theArgv[0]);
#ifndef __GLIBC__
  program_invocation_short_name = getApplicationValue (theArgv[0]);
#endif  
  [arguments setAppName: program_invocation_short_name];
  [arguments setAppModeString: "default"];

  argp_parse (argp, theArgc, theArgv, 0, 0, arguments);
  
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
  const char *signatureFile = SIGNATURE_FILE;
  int len = strlen (swarmPrefix) + strlen (swarm_version) + 1 + strlen (signatureFile) + 1;
  char *swarmVersionPathBuf = xmalloc (len);
  char *p, *swarmPath;
  
  p = stpcpy (swarmVersionPathBuf, swarmPrefix);
  p = stpcpy (p, swarm_version);
  p = stpcpy (p, "/");
  p = stpcpy (p, signatureFile);
  
  swarmPath = findDirectory (arguments, swarmVersionPathBuf);
  if (swarmPath == NULL)
    swarmPath = findDirectory (arguments, "swarm/" SIGNATURE_FILE);
  if (swarmPath)
    return dropDirectory (swarmPath);
  else
    return NULL;
}

- (const char *)getSwarmHome
{
  if (swarmHome == NULL)
    if ((swarmHome = getenv ("SWARMHOME")) == NULL)
      swarmHome = findSwarm (self);
  return swarmHome;
}

@end

