// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Arguments.h>
#import <defobj.h> // STRDUP, SSTRDUP, ZSTRDUP, arguments
#include <misc.h> // getenv, access, stpcpy, stat, dropdir

#include <swarmconfig.h> // SYSCONFDIR, HAVE_ARGP_H
#ifdef HAVE_ARGP_H
#include <argp.h>
#else
#include <misc/argp.h>
#endif

#ifndef __GLIBC__
const char *program_invocation_name;
const char *program_invocation_short_name;
#endif

#ifdef __CYGWIN__
#include <sys/cygwin.h> // cygwin32_conv_to_full_win32_path
#include <windows.h> // FindFirstFile
#undef BOOL
#endif

#undef __int64
#undef interface
#import <defobj/directory.h> // JAVA_APPNAME

#import "../defobj/internal.h"

#define VARCHAR(ch) (isAlnum (ch) || ((ch) == '_'))

#define SIGNATURE_FILE "swarmconfig.h"

#ifdef INCLUDESUBDIR
#define SIGNATURE_SUBPATH "include/" INCLUDESUBDIR "/"
#else
#define SIGNATURE_SUBPATH "include/"
#endif

#define SIGNATURE_PATH SIGNATURE_SUBPATH SIGNATURE_FILE

#include "version.h"

const char *swarm_version = SWARM_VERSION;

externvardef id arguments;

#define OPT_INHIBIT_ARCHIVER_LOAD 1

static struct argp_option base_options[] = {
  {"varyseed", 's', 0, 0, "Run with a random seed", 0},
  {"batch", 'b', 0, 0, "Run in batch mode", 1},
  {"mode", 'm', "MODE", 0, "Specify mode of use (for archiving)", 2},
  {"show-current-time", 't', 0, 0, "Show current time in control panel", 3},
  {"no-init-file", OPT_INHIBIT_ARCHIVER_LOAD, 0, 0, "Inhibit loading of ~/.swarmArchiver", 4},
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
    char *newStr = SSTRDUP (appStr);
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

  return [arguments parseKey: key arg: arg];
}

@implementation Arguments_c

PHASE(Creating)

+ createBegin: aZone
{
  Arguments_c *obj = [super createBegin: aZone];
  
  obj->argp = xmalloc (sizeof (struct argp));
  obj->argp->options = NULL;
  [obj addOptions: base_options];
  obj->argp->parser = parse_opt;
  obj->argp->args_doc = NULL;
  obj->argp->doc = NULL;
  obj->argp->children = NULL;
  obj->argp->help_filter = NULL;

  obj->defaultAppConfigPath = "./";
  obj->defaultAppDataPath = "./";

  obj->optionFunc = NULL;

  return obj;
}


- setBugAddress: (const char *)theBugAddress
{
  bugAddress = theBugAddress;
  return self;
}

- setVersion: (const char *)theVersion
{
  version = theVersion;
  return self;
}

- setInhibitArchiverLoadFlag: (BOOL)theInhibitArchiverLoadFlag
{
  inhibitArchiverLoadFlag = theInhibitArchiverLoadFlag;
  return self;
}

static const char *
strip_quotes (const char *argv0)
{
  if (*argv0 == '"')
    {
      char *ptr = SSTRDUP (argv0 + 1);
      size_t len = strlen (ptr);
      
      if (ptr[len - 1] == '"')
	ptr[len - 1] = '\0';
      return ptr;
    }
  else
    return SSTRDUP (argv0);
}

#define STRINGIFY(sym) #sym
#define STRINGIFYSYM(sym) STRINGIFY(sym)
#define JAVA_APPNAME_STRING STRINGIFYSYM(JAVA_APPNAME)
- createEnd
{
  const char *argv0 = strip_quotes (argv[0]);

  if (applicationName == NULL)  
    [self setAppName: getApplicationValue (argv0)];
  
#ifndef __GLIBC__
  program_invocation_short_name = applicationName;
  program_invocation_name = argv0;
#endif

  executablePath = ((strcmp (applicationName, JAVA_APPNAME_STRING) == 0)
                    ? NULL
                    : find_executable (argv0));

  if (version == NULL)
    version = "[no application version]";
  {
    const char *swarmstr = " (Swarm ";
    char *buf = xmalloc (strlen (applicationName) + 1 + 
                         strlen (version) + strlen (swarmstr) +
                         strlen (swarm_version) + 1 + 1);
    char *p;

    p = stpcpy (buf, applicationName);
    p = stpcpy (p, " ");
    p = stpcpy (p, version);
    p = stpcpy (p, swarmstr);
    p = stpcpy (p, swarm_version);
    p = stpcpy (p, ")");
    argp_program_version = buf;
  }
  if (bugAddress == NULL)
    {
      const char *bugstr = "bug-";
      const char *address = "@[none set]";
      char *buf = xmalloc (strlen (bugstr) + strlen (applicationName) + 
                           strlen (address) + 1);
      char *p;
      
      p = stpcpy (buf, bugstr);
      p = stpcpy (p, applicationName);
      p = stpcpy (p, address);
      argp_program_bug_address = buf;
    }
  else
    argp_program_bug_address = bugAddress;

  argp_parse (argp, argc, argv, 0, 0, self);
  return [super createEnd];
}

+ createArgc: (int)theArgc
        Argv: (const char **)theArgv
     appName: (const char *)theAppName
     version: (const char *)theVersion
  bugAddress: (const char *)theBugAddress
     options: (struct argp_option *)options
  optionFunc: (int (*) (int key, const char *arg))anOptionFunc
{
  Arguments_c *argobj = [self createBegin: globalZone];
  
  [argobj setArgc: theArgc Argv: theArgv];
  if (options)
    [argobj addOptions: options];
  [argobj setOptionFunc: anOptionFunc];
  [argobj setAppModeString: "default"];
  [argobj setBugAddress: theBugAddress];
  [argobj setVersion: theVersion];
  [argobj setAppName: theAppName];
  return [argobj createEnd];
}

- addOptions: (struct argp_option *)newoptions
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
  argp->options = options;
  return self;
}

- (int)parseKey: (int)key arg: (const char *)arg
{
  if (optionFunc)
    {
      if (optionFunc (key, arg) != ARGP_ERR_UNKNOWN)
        return 0;
    }
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
    case OPT_INHIBIT_ARCHIVER_LOAD:
      [self setInhibitArchiverLoadFlag: YES];
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

- setOptionFunc: (int (*) (int key, const char *arg))anOptionFunc
{
  optionFunc = anOptionFunc;
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
  return SSTRDUP (path);
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

- (BOOL)getInhibitArchiverLoadFlag
{
  return inhibitArchiverLoadFlag;
}

- (const char *)getAppName
{
  return applicationName;
}

- (const char *)getExecutablePath
{
  return executablePath;
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
findDirectory (id arguments, const char *directoryName)
{
  const char *exePath = [arguments getExecutablePath];

  if (exePath)
    {
      id aZone = [arguments getZone];
      char *pathBuf = ZSTRDUP (aZone, exePath);

      while (dropdir (pathBuf))
	{
	  char *swarmPathBuf =
            [aZone alloc: strlen (pathBuf) + strlen (directoryName) + 1];
	  
	  stpcpy (stpcpy (swarmPathBuf, pathBuf), directoryName);
	  if (access (swarmPathBuf, F_OK) != -1)
	    {
	      ZFREEBLOCK (aZone, pathBuf);
	      return swarmPathBuf;
	    }
	  ZFREEBLOCK (aZone, swarmPathBuf);
	}
      ZFREEBLOCK (aZone, pathBuf);
      return NULL;
    }
  else
    return NULL;
}

static unsigned
countSlashes (const char *path)
{
  unsigned count = 0;
  char *newPath = SSTRDUP (path);
  char *scratchPath = newPath;

  while ((scratchPath = dropdir (scratchPath))) count++;
  SFREEBLOCK (newPath);
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
        swarmPath = dropdir (swarmPath);
      return swarmPath;
    }
  else
    return NULL;
}

static const char *expandvars (const char *path);

static const char *
getSwarmPrefix (void)
{
  const char *swarmPrefix;

  if ((swarmPrefix = expandvars (getenv ("SWARMHOME"))) == NULL)
    {
      const char *expanded_path;

      expanded_path = expandvars (PREFIX);
      
      if (access (expanded_path, F_OK) == -1)
        swarmPrefix = NULL;
      else
        swarmPrefix = expanded_path;
    }
  return swarmPrefix;
}

static const char *
getnenv (const char *strptr, size_t len)
{
  char buf[len + 1];

  strncpy (buf, strptr, len);
  buf[len] = '\0';
  if (strcmp (buf, "prefix") == 0)
    return getSwarmPrefix ();
  else
    return getenv (buf);
}

static const char *
expandvars (const char *path)
{
  const char *p = path;
  size_t len = 0;
  
  if (path == NULL)
    return NULL;

  while (*p)
    {
      if (*p == '$')
        {
          p++;
          if (*p == '{')
            p++;
          {
            const char *varname = p;
            unsigned varlen = 0;
            
            while (VARCHAR (*p))
              {
                varlen++;
                p++;
              }
            {
              const char *ptr = getnenv (varname, varlen);

              if (ptr)
                len += strlen (ptr);
            }
          }
          if (*p == '}')
            p++;
        }
      else
        {
          len++;
          p++;
        }
    } 
  p = path;
  {
    char *ep, *expanded_path;

    ep = expanded_path = xmalloc (len + 1);
    while (*p)
      {
        if (*p == '$')
          {
            p++;
            if (*p == '{')
              p++;
            {
              const char *varname = p;
              unsigned varlen = 0;
              
              while (VARCHAR (*p))
                {
                  varlen++;
                  p++;
                }
              {
                const char *ptr = getnenv (varname, varlen);
                
                if (ptr)
                  ep = stpcpy (ep, ptr);
              }
            }
            if (*p == '}')
              p++;
          }
        else
          *ep++ = *p++;
      }
    *ep = '\0';
    return expanded_path;
  }
}

- (const char *)_getSwarmHome_: (BOOL)ignoreEnvFlag
{
  if (ignoreEnvFlag)
    return findSwarm (self);
  else
    {
      if (swarmHome == NULL)
	{
          if ((swarmHome = getSwarmPrefix ()) == NULL)
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
          if (swarmHome)
            {
              char sigPathBuf[(strlen (swarmHome) + 1 +
                               strlen (SIGNATURE_PATH) + 1)];
              char *p;
              
              p = stpcpy (sigPathBuf, swarmHome);
              p = stpcpy (p, "/");
              p = stpcpy (p, SIGNATURE_PATH);
              
              if (access (sigPathBuf, F_OK) == -1)
                swarmHome = NULL;

            }
          if (swarmHome == NULL)
            swarmHome = findSwarm (self);
        }  
      return swarmHome;
    }
}

- (const char *)getSwarmHome
{
  return [self _getSwarmHome_: NO];
}

- (const char *)_getPath_: (const char *)fixed subpath: (const char *)subpath
{
  struct stat buf;
  
  fixed = expandvars (fixed);
  if (stat (fixed, &buf) != -1)
    {
      if (buf.st_mode & S_IFDIR)
        return fixed;
    }
  else
    {
      const char *home = [self _getSwarmHome_: ignoringEnvFlag];
      if (home)
        {
          const char *newHome = ensureEndingSlash (home);
          char *buf = xmalloc (strlen (newHome) + strlen (subpath) + 1), *p;
            
          p = stpcpy (buf, newHome);
          stpcpy (p, subpath);
          SFREEBLOCK (newHome);
          return buf;
        }
    }
  return NULL;
}

- (const char *)getConfigPath
{
  return [self _getPath_: SYSCONFDIR "/swarm/" subpath: "etc/swarm/"];
}

- (const char *)getDataPath
{
  return [self _getPath_: DATADIR "/swarm/" subpath: "share/swarm/"];
}

#ifdef __CYGWIN__
static char *
stripSlash (char *buf)
{
  size_t len = strlen (buf);
  
  if (buf[len - 1] == '\\')
    buf[len - 1] = '\0';
  return buf;
}

static const char *
convertToLongPath (const char *path)
{
  WIN32_FIND_DATA findData;
  char newPath[MAXPATHLEN + 1], *p;
  char buf[MAXPATHLEN + 1];
  unsigned count = 0;

  strcpy (buf, path);
  do
    {
      if (FindFirstFile (stripSlash (buf), &findData) == 0)
	abort ();
      if (buf[3] == '\0')
	break;
      count++;
    }
  while (dropdir (buf));

  {
    const char *components[count];
    unsigned i = count;
    
    strcpy (buf, path);
    do
      {
	i--;
	if (FindFirstFile (stripSlash (buf), &findData) == 0)
	  abort ();
	if (buf[3] == '\0')
	  break;
	components[i] = SSTRDUP (findData.cFileName);
      }
    while (dropdir (buf));

    strncpy (newPath, path, 3);
    p = newPath + 3;
    
    for (i = 0; i < count; i++)
      {
	p = stpcpy (p, components[i]);
	*p++ = '\\';
      }
    *p = '\0';
  }
  return SSTRDUP (newPath);
}
#endif

- (BOOL)_runningFromInstall_
{
  if (!executablePath)
    return YES;
  else
    {
      const char *possibleHomeSrc;
      const char *homeSrc;
      BOOL ret = NO;
      char *executableBuf = STRDUP (executablePath);
      possibleHomeSrc = dropdir (dropdir (executableBuf));

      ignoringEnvFlag = NO;
    retry:
      homeSrc = [self _getSwarmHome_: ignoringEnvFlag];
      
      if (homeSrc && possibleHomeSrc)
	{
#ifdef __CYGWIN__
	  char possibleHome[MAXPATHLEN + 1];
	  char home[MAXPATHLEN + 1];
	  const char *longHome, *longPossibleHome;
	  
	  cygwin32_conv_to_full_win32_path (possibleHomeSrc, possibleHome);
	  cygwin32_conv_to_full_win32_path (homeSrc, home);
	  longHome = convertToLongPath (home);
	  longPossibleHome = convertToLongPath (possibleHome);
	  ret = strcmp (longHome, longPossibleHome) == 0;
#else
	  struct stat possibleHomeStatBuf, homeStatBuf;

	  if (stat (possibleHomeSrc, &possibleHomeStatBuf) != -1
	      && stat (homeSrc, &homeStatBuf) != -1)
	    ret = (possibleHomeStatBuf.st_ino == homeStatBuf.st_ino);
#endif
	  if (ret == NO && !ignoringEnvFlag)
	    {
	      ignoringEnvFlag = YES;
	      goto retry;
	    }
	}
      FREEBLOCK (executableBuf);
      return ret;
    }
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
  char *appConfigPath = STRDUP (defaultAppConfigPath);

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
  char *appDataPath = STRDUP (defaultAppDataPath);

  if ([self _runningFromInstall_])
    {
      const char *dataPath = [self getDataPath];

      if (dataPath)
        return [self _appendAppName_: dataPath];
    }
  return appDataPath;
}

@end

