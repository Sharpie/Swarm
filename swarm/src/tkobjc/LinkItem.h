// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompositeItem.h>
#import <gui.h>

@interface LinkItem: CompositeItem <LinkItem>
{
  id from, to;
  BOOL directedFlag;
  const char *line1, *line2;
}

- setDirectedFlag: (BOOL)d;
- setFrom: (id <NodeItem>)from;
- setTo: (id <NodeItem>)to;
- (void)createItem;

- (void)setColor: (const char *)aColor;
- (void)update;

@end
