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

#include <swarmconfig.h> // SYSCONFDIR, HAVE_ARGP_H, CONFIG_DATADIR

#import <defobj/Arguments.h>
#import <defobj.h> // STRDUP, SSTRDUP, ZSTRDUP, arguments
#import <defobj/defalloc.h> // getZone
#include <misc.h> // getenv, access, stpcpy, stat, dropdir, isAlnum

#ifdef HAVE_ARGP_H
#include <argp.h>
#else
#include <misc/argp.h>
#endif


#ifdef HAVE_JDK
#include "java.h"
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

#define VARCHAR(ch) ({ char _ch = ch; (isAlnum (_ch) || (_ch == '_')); })

#define SIGNATURE_FILE "Swarm/swarmconfig.h"

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
  {"varyseed", 's', 0, 0, "Select random number seed from current time", 0},
  {"seed", 'S', "INTEGER",0, "Specify seed for random numbers", 1},
  {"batch", 'b', 0, 0, "Run in batch mode", 2},
  {"mode", 'm', "MODE", 0, "Specify mode of use (for archiving)", 3},
  {"show-current-time", 't', 0, 0, "Show current time in control panel", 4},
  {"no-init-file", OPT_INHIBIT_ARCHIVER_LOAD, 0, 0, "Inhibit loading of ~/.swarmArchiver", 5},
  {"verbose", 'v', 0, 0, "Activate verbose messages", 6},
 
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

  // This is here because objc_lookup_class doesn't check for
  // foreign targets.  It is one of the few cases where Swarm
  // sends a message outside of the Action framework.

  id fa = [FArguments createBegin: getCZone (scratchZone)];
  id fc;
  error_t ret;
#ifdef HAVE_JDK
  jobject jobj = 0;

  jobj = SD_JAVA_FIND_OBJECT_JAVA (arguments);
  if (jobj)
    [fa setLanguage: LanguageJava];
#endif
  [fa addInt: key];
  [fa addString: arg];
  [fa setObjCReturnType: _C_INT];
  fa = [fa createEnd];
  fc = [FCall createBegin: getCZone (scratchZone)];
  [fc setArguments: fa];
#ifdef HAVE_JDK
  if (jobj)
    [fc setJavaMethodFromName: "parseKey$arg" inObject: jobj];
  else
#endif
    [fc setMethodFromSelector: M(parseKey:arg:) inObject: arguments];
  fc = [fc createEnd];

  [fc performCall];
  ret = ((types_t *) [fc getResult])->sint;
  [fc drop];
  [fa drop];
  return ret;
}

@implementation Arguments_c

PHASE(Creating)

+ createBegin: aZone
{
  Arguments_c *obj = [super createBegin: aZone];
  
  obj->argp = xmalloc (sizeof (struct argp));
#define ARGP_(obj) ((struct argp *)(obj->argp))
#define ARGP ((struct argp *)argp)
  ARGP_(obj)->options = NULL;
  [obj addOptions: base_options];
  ARGP_(obj)->parser = parse_opt;
  ARGP_(obj)->args_doc = NULL;
  ARGP_(obj)->doc = NULL;
  ARGP_(obj)->children = NULL;
  ARGP_(obj)->help_filter = NULL;
  obj->appModeString = "default";

  obj->defaultAppConfigPath = "./";
  obj->defaultAppDataPath = "./";

  obj->optionFunc = NULL;

  return obj;
}


- setBugAddress: (const char *)theBugAddress
{
  bugAddress = theBugAddress ? SSTRDUP (theBugAddress) : NULL;
  return self;
}

- setVersion: (const char *)theVersion
{
  version = theVersion ? SSTRDUP (theVersion) : NULL;
  return self;
}

- setInhibitArchiverLoadFlag: (BOOL)theInhibitArchiverLoadFlag
{
  inhibitArchiverLoadFlag = theInhibitArchiverLoadFlag;
  return self;
}

- setInhibitExecutableSearchFlag: (BOOL)theInhibitExecutableSearchFlag
{
  inhibitExecutableSearchFlag = theInhibitExecutableSearchFlag;
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
    case 'v':
      [self setVerboseFlag: YES];
      break;
    case OPT_INHIBIT_ARCHIVER_LOAD:
      [self setInhibitArchiverLoadFlag: YES];
      break;
    case 'S':
      [self setFixedSeed: strtoul (arg, NULL, 10)];
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

- (void)addOption: (const char *)name key: (int)key arg: (const char *)arg flags: (int)flags doc: (const char *)doc group: (int)group
{
  struct argp_option options[2];

  options[0].name = SSTRDUP (name);
  options[0].key = key;
  options[0].arg = arg ? SSTRDUP (arg) : NULL;
  options[0].flags = flags;
  options[0].doc = doc ? SSTRDUP (doc) : "[none]";
  options[0].group = group;
  options[1].name = NULL;
  [self addOptions: options];
}

- (void)addOptions: (struct argp_option *)newoptions
{
  unsigned exist_count = 0, total_count = 0, new_count = 0;
  struct argp_option *options = (struct argp_option *)ARGP->options;
  
  if (ARGP->options)
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
  if (ARGP->options)
    options = xrealloc ((void *)ARGP->options,
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
  ARGP->options = options;
}

#define STRINGIFY(sym) #sym
#define STRINGIFYSYM(sym) STRINGIFY(sym)
- createEnd
{
  const char *argv0;

  if (argc == 0) {
    argv = xmalloc (sizeof (char *));
    argv[0] = "unknown";
    argc = 1;
  }
  argv0 = argv[0];

  argv0 = strip_quotes (argv0);

  if (applicationName == NULL)  
    [self setAppName: getApplicationValue (argv0)];
  
#ifndef __GLIBC__
  program_invocation_short_name = applicationName;
  program_invocation_name = argv0;
#endif

  executablePath = (inhibitExecutableSearchFlag
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

  argp_parse (argp, argc, argv, 0, &lastArgIndex, self);
  return [super createEnd];
}

+ createArgc: (int)theArgc
        Argv: (const char **)theArgv
     appName: (const char *)theAppName
     version: (const char *)theVersion
  bugAddress: (const char *)theBugAddress
     options: (struct argp_option *)options
  optionFunc: (int (*) (int key, const char *arg))anOptionFunc
inhibitExecutableSearchFlag: (BOOL)theInhibitExecutableSearchFlag
{
  Arguments_c *argobj = [self createBegin: globalZone];
  
  [argobj setArgc: theArgc Argv: theArgv];
  if (options)
    [argobj addOptions: options];
  [argobj setOptionFunc: anOptionFunc];
  [argobj setBugAddress: theBugAddress];
  [argobj setVersion: theVersion];
  [argobj setAppName: theAppName];
  [argobj setInhibitExecutableSearchFlag: theInhibitExecutableSearchFlag];
  return [argobj createEnd];
}

PHASE(Setting)

- setArgc: (unsigned)theArgc Argv: (const char **)theArgv
{
  int i;
  
  argc = theArgc;
  argv = [getZone (self) alloc: sizeof (const char *) * argc];

  for (i = 0; i < argc; i++)
    argv[i] = STRDUP (theArgv[i]);

  return self;
}

- setAppName: (const char *)theApplicationName;
{
  applicationName = STRDUP (theApplicationName);

  return self;
}

- setAppModeString: (const char *)theAppModeString
{
  appModeString = STRDUP (theAppModeString);
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

  if (varySeedFlag)
    fixedSeedFlag = NO;

  return self;
}

- setVerboseFlag: (BOOL)theVerboseFlag
{
  verboseFlag = theVerboseFlag;
  
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

- setFixedSeed: (unsigned)theFixedSeed
{
  fixedSeed = theFixedSeed;
  fixedSeedFlag = YES;
  varySeedFlag = NO;

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

- (BOOL)getFixedSeedFlag
{
  return fixedSeedFlag;
}

- (BOOL)getVerboseFlag
{
  return verboseFlag;
}

- (BOOL)getShowCurrentTimeFlag
{
  return showCurrentTimeFlag;
}

- (BOOL)getInhibitArchiverLoadFlag
{
  return inhibitArchiverLoadFlag;
}

- (unsigned)getFixedSeed
{
  return fixedSeed;
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

- (int)getLastArgIndex
{
  return lastArgIndex;
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
      const char *expanded_path = expandvars (PREFIX);
      
      if (access (expanded_path, F_OK) == -1)
        swarmPrefix = NULL;
      else
        swarmPrefix = expanded_path;
    }
  if (swarmPrefix)
    { 
      unsigned len = strlen (swarmPrefix);
    
      if (swarmPrefix[len - 1] != '/')
	{
	  char *str = xmalloc (len + 2), *p;
	  
	  p = stpcpy (str, swarmPrefix);
	  p = stpcpy (p, "/");
	  swarmPrefix = str;
	}
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
          swarmHome = getSwarmPrefix ();
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
  return [self _getPath_: CONFIG_DATADIR "/swarm/" subpath: "share/swarm/"];
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
  if (inhibitExecutableSearchFlag)
    return NO;
  else if (!executablePath)
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



