// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/CanvasItem.h>

@interface TextItem: CanvasItem
{
  const char *text;
  const char *font;
  int x, y ;
}

- setX: (int) x Y: (int) y;
- setText: (const char *)the_text;
- setText: (const char *)the_text usingFont: (const char *)the_font;
- setFont: (const char *)the_font;
- createItem;
@end
