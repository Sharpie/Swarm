// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <defobj.h>
#import <collections.h>

#include <swarmconfig.h>

#define ARCHIVER_FUNCTION_NAME "archiver"

const char *defaultPath (const char *swarmArchiver);
const char *defaultAppPath (const char *appDataPath, const char *appName,
                            const char *suffix);

@interface ArchiverObject: CreateDrop
{
  id expr;
  id object;
}
+ create: aZone withExpr: valexpr;
+ create: aZone withObject: theObj;
- setExpr: valexpr;
- getExpr;
- setObject: theObj;
- getObject;
@end

@interface Application: CreateDrop
{
  const char *name;
  id <Map> deepMap;
  id <Map> shallowMap;
}
+ createBegin: aZone;
- setName: (const char *)name;
- getDeepMap;
- getShallowMap;
@end

@interface Archiver_c: CreateDrop_s <Archiver>
{
  id currentApplicationKey;
  id <Map> applicationMap;
  id inStreamZone;
  BOOL inhibitLoadFlag;
  BOOL systemArchiverFlag;
  const char *path;
@public
  id <List> classes;
  id <List> instances;
}
+ createBegin: aZone;
+ create: aZone setPath: (const char *)thePath;

- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;
- setPath: (const char *)path;
- setSystemArchiverFlag: (BOOL)systemArchiverFlag;
- setDefaultPath;
- setDefaultAppPath;

- createAppKey: (const char *)appName mode: (const char *)modeName;
- ensureApp: appKey;

- getApplication;

- registerClient: client;
- unregisterClient: client;

- (unsigned)countObjects: (BOOL)deepFlag;
- updateArchiver;
- save;

- getObject: (const char *)key;
- getWithZone: aZone object: (const char *)key;
- putDeep: (const char *)key object: object;
- putShallow: (const char *)key object: object;

@end
