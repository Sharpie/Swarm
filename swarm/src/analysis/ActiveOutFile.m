// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/ActiveOutFile.h>
#import <simtools/OutFile.h>

@implementation ActiveOutFile
PHASE(Creating)

- setFileObject: aFileObj
{
  file = aFileObj;
  return self;
}

- setHDF5Dataset: (id <HDF5>)hdf5Obj
{
  hdf5Dataset = hdf5Obj;
  return self;
}

- setDataFeed: d
{
  dataFeed = d;
  return self;
}

- createEnd
{
  if (!(file || hdf5Dataset) || !dataFeed)
    [InvalidCombination raiseEvent: "ActiveOutFile not initialized properly"];
  [self setProbedClass: getClass (dataFeed)];
  [super createEnd];
  return self;
}

PHASE(Setting)
PHASE(Using)

- step
{
  if (file)
    {
      [file putDouble: [self doubleDynamicCallOn: dataFeed]];
      [file putNewLine];
    }
  else
    [hdf5Dataset addDoubleToVector: [self doubleDynamicCallOn: dataFeed]];
  return self;
}

- (void)drop
{
  if (file)
    [file drop];
  [super drop];
}

@end
