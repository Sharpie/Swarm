// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#include <swarmconfig.h>

#define SWARMARCHIVER_LISP ".swarmArchiver"
#define SWARMARCHIVER_LISP_SUFFIX ".scm"

extern id lispArchiver;

@interface LispArchiver_c: Archiver_c <LispArchiver>
{
}
+ createBegin: aZone;
+ create: aZone setPath: (const char *)path;
- setDefaultPath;
- setDefaultAppPath;

- createEnd;

- lispLoadArchiver: expr;

- save;

- putDeep: (const char *)key object: object;
- putShallow: (const char *)key object: object;
- _getWithZone_: aZone _object_: (const char *)key;
- getObject: (const char *)key;
- getWithZone: aZone object: (const char *)key;

@end
