// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

#import <awtobjc/global.h>
#import <awtobjc/Widget.h>
#import <awtobjc/NodeItem.h>

@interface RectangleNodeItem: NodeItem
{
#ifdef USE_JAVA
  id shape;	// underlying graphic representation
#endif
}

#ifdef USE_JAVA
// java composites are backed by some sort of java object
- getObj;
#endif

@end


