// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/NodeItem.h>
#import <awtobjc/CompositeItem.h>

@interface LinkItem: CompositeItem
{
  id from, to;
  const char *line1, *line2;
}

- setFrom: from;
- setTo: to;
- createItem;

- setColor: (const char *)aColor;
- update;

@end
