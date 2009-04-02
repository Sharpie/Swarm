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

#import <defobj/Archiver.h>

#import <collections.h>
#import <defobj.h> // STRDUP
#import <defobj/defalloc.h> // getZone
#include <misc.h> // getenv, stpcpy

const char *
defaultPath (const char *swarmArchiver)
{
  const char *home = getenv ("HOME");

  if (!home)
    home = getenv ("SWARMHOME");

  if (home)
    {
      size_t homelen = strlen (home);
      char *buf =
        [scratchZone alloc: homelen + 1 + strlen (swarmArchiver) + 1];
      char *p = stpcpy (buf, home);

      if (homelen == 0 || home[homelen - 1] != '/')
        p = stpcpy (p, "/");

      p = stpcpy (p, swarmArchiver);
      
      return buf;
    }
  return NULL;
}

const char *
defaultAppPath (const char *appDataPath, const char *appName,
                const char *suffix)
{
  char *buf =
    [scratchZone
      alloc: 
        strlen (appDataPath) + strlen (appName) + strlen (suffix) + 1];
  char *p;
  
  p = stpcpy (buf, appDataPath);
  p = stpcpy (p, appName);
  p = stpcpy (p, suffix);
  return buf;
}

@implementation Archiver_c
PHASE(Creating)

+ createBegin: aZone
{
  Archiver_c *newArchiver = [super createBegin: aZone];
 
  newArchiver->applicationMap = [Map create: aZone];
  newArchiver->classes = [List create: aZone];
  newArchiver->instances = [List create: aZone];
  newArchiver->path = NULL;
  newArchiver->inhibitLoadFlag = NO;
  newArchiver->systemArchiverFlag = NO;
  
  return newArchiver;
}

+ create: aZone setPath: (const char *)thePath
{
  Archiver_c *obj = [self createBegin: aZone];
  [obj setPath: thePath];
  return [obj createEnd];
}

- setInhibitLoadFlag: (BOOL)theInhibitLoadFlag
{
  inhibitLoadFlag = theInhibitLoadFlag;
  return self;
}

- setPath: (const char *)thePath
{
  path = STRDUP (thePath);
  return self;
}

- setDefaultPath
{
  raiseEvent(SubclassMustImplement, "");
  return self;
}

- setDefaultAppPath
{
  raiseEvent (SubclassMustImplement, "");
  return self;
}

- setSystemArchiverFlag: (BOOL)theSystemArchiverFlag
{
  systemArchiverFlag = theSystemArchiverFlag;
  return self;
}

- createEnd
{
  [super createEnd];
  currentApplicationKey = [self createAppKey: [arguments getAppName]
                                mode: [arguments getAppModeString]];
  return self;
}

PHASE(Setting)

PHASE(Using)

- createAppKey: (const char *)appName mode: (const char *)modeName
{
  id appKey = [String create: getZone (self) setC: appName];

  [appKey catC: "/"];
  [appKey catC: modeName];
  return appKey;
}

- getApplication
{
  return [applicationMap at: currentApplicationKey];
}

- (void)registerClient: client
{
  if (![client isInstance])
    {
      if (![classes contains: client])
        [classes addLast: client];
    }
  else if (![instances contains: client])
    [instances addLast: client];
}

- (void)unregisterClient: client
{
  if (![client isInstance])
    [classes remove: client];
  else
    [instances remove: client];
}

- (void)putDeep: (const char *)key object: object
{
  raiseEvent (SubclassMustImplement, "");
}

- (void)putShallow: (const char *)key object: object
{
  raiseEvent (SubclassMustImplement, "");
}

- getObject: (const char *)key
{
  raiseEvent (SubclassMustImplement, "");
  return self;
}

- getWithZone: aZone key: (const char *)key
{
  raiseEvent (SubclassMustImplement, "");
  return self;
}

- (void)updateArchiver
{
  id <Index> index;
  id item;
  IMP func = swarm_class_getMethodImplementation (id_CreatedClass_s, M(updateArchiver:));
  
  index = [classes begin: getZone (self)];
  while ((item = [index next]))
    func (item, M(updateArchiver:), self);
  [index drop];
  [instances forEach: @selector (updateArchiver:) : self];
}

- (void)sync
{
  raiseEvent (SubclassMustImplement, "");
}

- (void)drop
{
  if (path)
    FREEBLOCK (path);
  [applicationMap deleteAll];
  [applicationMap drop];
  [classes drop];
  [instances drop];

  [super drop];
}

@end



