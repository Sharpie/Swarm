// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

#import <tkobjc/NodeItem.h>
#import <tkobjc/CanvasItem.h>

@interface LinkItem: CanvasItem {
  id from, to ;
  char *line1,*line2 ;
}

-update ;
-setFrom: from ;
-setTo: to ;
-createItem ;
@end
