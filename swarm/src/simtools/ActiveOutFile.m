// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ActiveOutFile.h>
#import <simtools/OutFile.h>

@implementation ActiveOutFile

- setFileObject: aFileObj
{
  theFile = aFileObj;
  return self;
}

- setDataFeed: d
{
  dataFeed = d;
  return self;
}

- createEnd
{
  if (theFile == nil || dataFeed == nil)
    [InvalidCombination raiseEvent: "ActiveOutFile not initialized properly"];
  [self setProbedClass: [dataFeed class]];
  [super createEnd];
  return self;
}

- step
{
  [theFile putDouble: [self doubleDynamicCallOn: dataFeed]];
  [theFile putNewLine];
  return self;
}

- (void)drop
{
  [theFile drop];
  [super drop];
}

@end
