// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Arguments.h>
#import <objectbase.h> // arguments
#import <collections.h> // String
#include <misc.h> // strdup, getenv, access, stpcpy, stat
#include <misc/argp.h>
#include <swarmconfig.h> // CONFPATH

#ifndef __GLIBC__
const char *program_invocation_name;
const char *program_invocation_short_name;
#endif

#define SIGNATURE_FILE "etc/swarm/Makefile.appl"

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
  {"show-current-time", 't', 0, 0, "Show current time in control panel", 3},
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

PHASE(Creating)

static struct argp *argp;

static error_t
parse_opt (int key, const char *arg, struct argp_state *state)
{
  id arguments = state->input;

  return [arguments parseKey: key arg: arg];
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

  obj->defaultSwarmAppConfigPath = "./";

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
    case 't':
      [self setShowCurrentTimeFlag: YES];
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

PHASE(Setting)

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

- setShowCurrentTimeFlag: (BOOL)theShowCurrentTimeFlag
{
  showCurrentTimeFlag = theShowCurrentTimeFlag;
  
  return self;
}

- setDefaultSwarmAppConfigPath: (const char *)path
{
  unsigned len = strlen (path);

  if (path[len - 1] != '/')
    {
      char *p, *buf;

      buf = xmalloc (len + 2);
      p = stpcpy (buf, path);
      stpcpy (p, "/");
      
      defaultSwarmAppConfigPath = buf;
    }
  else
    defaultSwarmAppConfigPath = path;

  return self;
}

PHASE(Using)

- (BOOL)getBatchModeFlag
{
  return batchModeFlag;
}

- (BOOL)getVarySeedFlag
{
  return varySeedFlag;
}

- (BOOL)getShowCurrentTimeFlag
{
  return showCurrentTimeFlag;
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

static unsigned
countSlashes (const char *path)
{
  unsigned count = 0;
  char *newPath = strdup (path);
  char *scratchPath = newPath;

  while ((scratchPath = dropDirectory (scratchPath))) count++;
  xfree (newPath);
  return count;
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
    {
      unsigned i, dropCount = countSlashes (SIGNATURE_FILE) + 1;
      
      for (i = 0; i < dropCount; i++)
        swarmPath = dropDirectory (swarmPath);
      return swarmPath;
    }
  else
    return NULL;
}

- (const char *)getSwarmHome
{
  if (swarmHome == NULL)
    {
      if ((swarmHome = getenv ("SWARMHOME")) == NULL)
        swarmHome = findSwarm (self);
      else
        {
          unsigned len = strlen (swarmHome);

          if (swarmHome[len - 1] != '/')
            {
              char *home = xmalloc (len + 2), *p;

              p = stpcpy (home, swarmHome);
              p = stpcpy (p, "/");
              swarmHome = home;
            }
        }
    }
  return swarmHome;
}

- (const char *)getSwarmConfigPath
{
  id configDir = [String create: [self getZone] setC: CONFPATH];
  id configTestFile;
  const char *ret;

  [configDir catC: "/"];
  configTestFile = [configDir copy: [self getZone]];
  [configTestFile catC: "Makefile.appl"];

  if (access ([configTestFile getC], R_OK) != -1)
    ret = strdup ([configDir getC]);
  else
    {
      const char *home = [self getSwarmHome];
      if (home)
        {
          id configNewDir = [String create: [self getZone] setC: home];
          if (home[strlen (home) - 1] != '/')
            [configNewDir catC: "/"];
          
          [configNewDir catC: "etc/swarm/"];
          ret = strdup ([configNewDir getC]);
          [configNewDir drop];
        }
      else
        ret = NULL;
    }
  [configDir drop];
  [configTestFile drop];
  return ret;
}

- (const char *)getSwarmAppConfigPath
{
  char *executablePath = strdup ([self getExecutablePath]);
  const char *possibleHome = dropDirectory (dropDirectory (executablePath));
  const char *home = [self getSwarmHome];
  char *appConfigPath = (char *)defaultSwarmAppConfigPath;

  if (home && possibleHome)
    {
      struct stat possibleHomeStatBuf, homeStatBuf;
      
      if (stat (possibleHome, &possibleHomeStatBuf) != -1
          && stat (home, &homeStatBuf) != -1)
        if (possibleHomeStatBuf.st_ino == homeStatBuf.st_ino)
          {
            const char *configPath = [self getSwarmConfigPath];
            const char *appName = [self getAppName];
            char *p;
            
            if (!configPath)
              return NULL;
            
            appConfigPath = xmalloc (strlen (configPath) +
                                     strlen (appName) + 2);
            p = stpcpy (appConfigPath, configPath);
            p = stpcpy (p, appName);
            p = stpcpy (p, "/");
          }
    }
  xfree (executablePath);
  
  return appConfigPath;
}

@end

