// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#include <swarmconfig.h>

#define SWARMARCHIVER_HDF5 "swarmArchiver.hdf"
#define SWARMARCHIVER_HDF5_SUFFIX ".hdf"

extern id hdf5Archiver;

@interface HDF5Archiver_c: Archiver_c <HDF5Archiver>
{
}

+ createBegin: aZone;
+ create: aZone setPath: (const char *)path;
- setDefaultPath;
- setDefaultAppPath;

- createEnd;

- (void)ensureApp: hdf5File;

- (void)putDeep: (const char *)key object: object;
- (void)putShallow: (const char *)key object: object;
- getObject: (const char *)key;
- getWithZone: aZone key: (const char *)key;

@end
