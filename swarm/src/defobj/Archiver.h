// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

@interface Archiver_c: CreateDrop_s <Archiver>
{
  id currentApplicationKey;
  id <Map> applicationMap;
  BOOL inhibitLoadFlag;
  BOOL systemArchiverFlag;
  const char *path;
@public
  id <List> classes;
  id <List> instances;
}
+ createBegin: aZone;
+ create: aZone setPath: (const char *)thePath;
- createEnd;

- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;
- setPath: (const char *)path;
- setSystemArchiverFlag: (BOOL)systemArchiverFlag;
- setDefaultPath;
- setDefaultAppPath;

- createAppKey: (const char *)appName mode: (const char *)modeName;

- getApplication;

- (void)registerClient: client;
- (void)unregisterClient: client;

- (void)updateArchiver;
- (void)sync;

- getObject: (const char *)key;
- getWithZone: aZone key: (const char *)key;
- (void)putDeep: (const char *)key object: object;
- (void)putShallow: (const char *)key object: object;

- (void)drop;

@end

