// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasItem.h>
#import <gui.h>

@interface TextItem: CanvasItem <TextItem>
{
  const char *text;
  const char *font;
  int x, y;
  BOOL centerFlag;
}

+ createBegin: aZone;
- setX: (int)x Y: (int)y;
- setText: (const char *)text;
- setFont: (const char *)font;
- setCenterFlag: (BOOL)centerFlag;
- createItem;
@end
