// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
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

- hdf5LoadObjectMap: hdfObj key: appKey;
- hdf5LoadArchiver: hdf5File;

- putDeep: (const char *)key object: object;
- putShallow: (const char *)key object: object;
- _getWithZone_: aZone _object_: (const char *)key;
- getObject: (const char *)key;
- getWithZone: aZone object: (const char *)key;

@end
