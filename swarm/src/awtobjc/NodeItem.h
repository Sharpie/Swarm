// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/CompositeItem.h>

@interface NodeItem: CompositeItem
{
  int x,y;
  const char *text;
  const char *item;
  const char *string;
}

- setX: (int)x Y: (int)y;
- (int)getX;
- (int)getY;
- setString: (const char *)string;
- setColor: (const char *)aColor;
- setBorderColor: (const char *)aColor;
- setBorderWidth: (int)aVal;
- createBindings;
@end

