// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Arguments.h>
#import <objectbase.h> // arguments
#include <misc.h> // strdup, getenv, access, stpcpy, stat
#include <misc/argp.h>
#include <swarmconfig.h> // CONFPATH

#ifndef __GLIBC__
const char *program_invocation_name;
const char *program_invocation_short_name;
#endif

#define SIGNATURE_FILE "Makefile.appl"
#define SIGNATURE_SUBPATH "etc/swarm/"
#define SIGNATURE_PATH "etc/swarm/" SIGNATURE_FILE

#include "version.h"

const char *swarm_version = SWARM_VERSION;

id arguments;

@implementation Arguments

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

  obj->defaultAppConfigPath = "./";
  obj->defaultAppDataPath = "./";

  return obj;
}

+ createArgc: (int)theArgc
        Argv: (const char **)theArgv
     version: (const char *)version
  bugAddress: (const char *)bugAddress
{
  id arguments = [self createBegin: globalZone];
  
  [arguments setArgc: theArgc Argv: theArgv];
  program_invocation_name = (char *)find_executable (theArgv[0]);
#ifndef __GLIBC__
  program_invocation_short_name = getApplicationValue (theArgv[0]);
#endif  
  [arguments setAppName: program_invocation_short_name];
  [arguments setAppModeString: "default"];
  if (version == NULL)
    version = "[no application version]";
  {
    const char *appName = [arguments getAppName];
    const char *swarmstr = " (Swarm ";
    char *buf = xmalloc (strlen (appName) + 1 + 
                         strlen (version) + strlen (swarmstr) +
                         strlen (swarm_version) + 1 + 1);
    char *p;

    p = stpcpy (buf, appName);
    p = stpcpy (p, " ");
    p = stpcpy (p, version);
    p = stpcpy (p, swarmstr);
    p = stpcpy (p, swarm_version);
    p = stpcpy (p, ")");
    argp_program_version = buf;
  }
  if (bugAddress == NULL)
    {
      const char *appName = [arguments getAppName];
      const char *bugstr = "bug-";
      const char *address = "@[none set]";
      char *buf = xmalloc (strlen (bugstr) + strlen (appName) + 
                           strlen (address) + 1);
      char *p;
      
      p = stpcpy (buf, bugstr);
      p = stpcpy (p, appName);
      p = stpcpy (p, address);
      argp_program_bug_address = buf;
    }
  else
    argp_program_bug_address = bugAddress;

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
    case 'm':
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

const char *
ensureEndingSlash (const char *path)
{
  unsigned len = strlen (path);

  if (path[len - 1] != '/')
    {
      char *p, *buf;
      
      buf = xmalloc (len + 2);
      p = stpcpy (buf, path);
      stpcpy (p, "/");
      return buf;
    }
  return strdup (path);
}

- setDefaultAppConfigPath: (const char *)path
{
  defaultAppConfigPath = ensureEndingSlash (path);

  return self;
}

- setDefaultAppDataPath: (const char *)path
{
  defaultAppDataPath = ensureEndingSlash (path);

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
      XFREE (swarmPathBuf);
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
  XFREE (newPath);
  return count;
}

static char *
findSwarm (id arguments)
{
  const char *swarmPrefix = "swarm-";
  const char *signatureFile = SIGNATURE_PATH;
  int len = strlen (swarmPrefix) + strlen (swarm_version) + 1 + strlen (signatureFile) + 1;
  char *swarmVersionPathBuf = xmalloc (len);
  char *p, *swarmPath;
  
  p = stpcpy (swarmVersionPathBuf, swarmPrefix);
  p = stpcpy (p, swarm_version);
  p = stpcpy (p, "/");
  p = stpcpy (p, signatureFile);
  
  swarmPath = findDirectory (arguments, swarmVersionPathBuf);
  if (swarmPath == NULL)
    swarmPath = findDirectory (arguments, "swarm/" SIGNATURE_PATH);
  if (swarmPath)
    {
      unsigned i, dropCount = countSlashes (SIGNATURE_PATH) + 1;
      
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

- (const char *)_getPath_: (const char *)fixed subpath: (const char *)subpath
{
  struct stat buf;
  
  if (stat (fixed, &buf) != -1)
    {
      if (buf.st_mode & S_IFDIR)
        return fixed;
    }
  else
    {
      const char *home = [self getSwarmHome];
      if (home)
        {
          const char *newHome = ensureEndingSlash (home);
          char *buf = xmalloc (strlen (newHome) + strlen (subpath) + 1), *p;
            
          p = stpcpy (buf, newHome);
          stpcpy (p, subpath);
          XFREE (newHome);
          return buf;
        }
    }
  return NULL;
}

- (const char *)getConfigPath
{
  return [self _getPath_: SYSCONFDIR subpath: SIGNATURE_SUBPATH];
}

- (const char *)getDataPath
{
  return [self _getPath_: DATADIR subpath: "share/swarm/"];
}

- (BOOL)_runningFromInstall_
{
  char *executablePath = strdup ([self getExecutablePath]);
  const char *possibleHome = dropDirectory (dropDirectory (executablePath));
  const char *home = [self getSwarmHome];
  BOOL ret = NO;

  if (home && possibleHome)
    {
      struct stat possibleHomeStatBuf, homeStatBuf;
      
      if (stat (possibleHome, &possibleHomeStatBuf) != -1
          && stat (home, &homeStatBuf) != -1)
        ret = (possibleHomeStatBuf.st_ino == homeStatBuf.st_ino);
    }
  XFREE (executablePath);
  return ret;
}

- (const char *)_appendAppName_: (const char *)basePath
{
  const char *appName = [self getAppName];
  char *p;
  char *path = xmalloc (strlen (basePath) + strlen (appName) + 2);
  p = stpcpy (path, basePath);
  p = stpcpy (p, appName);
  p = stpcpy (p, "/");

  return path;
}

- (const char *)getAppConfigPath
{
  char *appConfigPath = strdup (defaultAppConfigPath);

  if ([self _runningFromInstall_])
    {
      const char *configPath = [self getConfigPath];
  
      if (configPath)
        return [self _appendAppName_: configPath];
    }
  return appConfigPath;
}

- (const char *)getAppDataPath
{
  char *appDataPath = strdup (defaultAppDataPath);

  if ([self _runningFromInstall_])
    {
      const char *dataPath = [self getDataPath];

      if (dataPath)
        return [self _appendAppName_: dataPath];
    }
  return appDataPath;
}

@end

