// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

#import <tkobjc/NodeItem.h>
#import <tkobjc/CompositeItem.h>

@interface LinkItem: CompositeItem {
  id from, to ;
  char *line1,*line2 ;
}

-setFrom: from ;
-setTo: to ;
-createItem ;

-setColor: (char *) aColor ;
-update ;

@end
