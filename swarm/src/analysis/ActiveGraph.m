// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/ActiveGraph.h>
#import <objectbase.h>
#import <activity.h> // getCurrentTime
#import <gui.h>

// ActiveGraph: a graph that actively updates its own element when stepped.
@implementation ActiveGraph

PHASE(Creating)

- setElement: (id <GraphElement>)ge
{
  element = ge;
  return self;
}

- setDataFeed: d
{
  dataFeed = d;
  return self;
}

- createEnd
{
  if (element == nil || dataFeed == nil)
    [InvalidCombination raiseEvent: "ActiveGraph not initialized properly"];
  [self setProbedClass: [dataFeed class]] ;
  [super createEnd];
  return self;
}

PHASE(Using)

// add a new point, (currentTime, averageValue).
- step
{
  [element addX: (double)getCurrentTime ()
           Y: [self doubleDynamicCallOn: dataFeed]];
  return self;
}

@end
