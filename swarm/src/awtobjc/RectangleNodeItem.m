// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <awtobjc/RectangleNodeItem.h>
#ifdef USE_JAVA
#import <awtobjc/JavaItem.h>
#import <awtobjc/Canvas.h>
#endif

@implementation RectangleNodeItem

-createItem
{
#if 0
  shape = [[[JavaItem create: [self getZone]] 
	     initType: ITEM_RECT W: 20 H: 20] createEnd];
  [canvas placeObj: shape X: x-10 Y: y-10];
  [self createBindings];
#else
  abort ();
#endif

  return self;
}

// java composites are backed by some sort of java object
- getObj
{
  return shape;
}

@end
