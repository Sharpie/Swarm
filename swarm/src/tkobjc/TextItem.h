// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/CanvasItem.h>

@interface TextItem: CanvasItem {
  char *text ;
  int x, y ;
}

-setX: (int) x Y: (int) y ;
-setText: (char *) text ;
-createItem ;
@end
