// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis.h> // ActiveOutFile
#import <defobj.h> // HDF5
#import <objectbase/MessageProbe.h>

@interface ActiveOutFile: MessageProbe <ActiveOutFile>
{
  id file;
  id <HDF5> hdf5Dataset;
  id dataFeed;
}

- setFileObject: aFileObj;
- setHDF5Dataset: (id <HDF5>)hdf5Obj;
- setDataFeed: d;
- (void)step;
@end
