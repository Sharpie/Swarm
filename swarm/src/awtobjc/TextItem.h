// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

#import <awtobjc/global.h>
#import <awtobjc/Widget.h>
#import <awtobjc/CanvasItem.h>

@interface TextItem: CanvasItem
{
  const char *text;
  int x, y;
}

- setX: (int)x Y: (int)y;
- setText: (const char *)text;
- createItem;
@end
