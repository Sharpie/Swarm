// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/global.h>
#import <awtobjc/OvalNodeItem.h>
#import <awtobjc/Canvas.h>
#import <awtobjc/JavaItem.h>

@implementation OvalNodeItem

- createItem
{
  shape = [JavaItem createBegin: [self getZone]];
  [shape addInt: ITEM_OVAL];
  [shape addInt: 20];
  [shape addInt: 20];
  shape = [shape createEnd];
  
  [canvas placeObj: shape X: x - 10 Y: y - 10];
 
  [self createBindings];

  return self;
}

@end
