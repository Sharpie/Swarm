// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ActiveOutFile.h>
#import <simtools/OutFile.h>
#import <activity.h>

//S: An object that actively updates its file stream when updated.

//D: This is the file I/O equivalent of ActiveGraph: it takes an OutFile 
//D: object, a target (datafeed) object, and a selector, which it uses to
//D: extract data from the object and send it to the file. 
@implementation ActiveOutFile

//M: The setFileObject: method sets the file object to which the data will be
//M: sent.
- setFileObject: aFileObj
{
  theFile = aFileObj;
  return self;
}

//M: The setDataFeed: method sets the object that will be probed for data.
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

//M: The step method fires the probe, reads the value from the object, and 
//M: sends the value to the file.
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
