// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#include <swarmconfig.h>

#define SWARMARCHIVER_LISP ".swarmArchiver"
#define SWARMARCHIVER_LISP_SUFFIX ".scm"

extern id lispArchiver;

@interface Application: CreateDrop
{
  const char *name;
  id <Map> streamMap;
}
+ createBegin: aZone;
- setName: (const char *)name;
- getStreamMap;
@end

@interface LispArchiver_c: Archiver_c <LispArchiver>
{
  id inStreamZone;
}
+ createBegin: aZone;
+ create: aZone setPath: (const char *)path;
- setDefaultPath;
- setDefaultAppPath;

- createEnd;
- ensureApp: appKey;

- (BOOL)_load_;
- (void)lispLoadArchiver: expr;

- (void)sync;

- (void)putDeep: (const char *)key object: object;
- (void)putShallow: (const char *)key object: object;
- _getWithZone_: aZone key: (const char *)key;
- getObject: (const char *)key;
- getWithZone: aZone key: (const char *)key;

- (void)drop;

@end
