// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CompositeItem.h>
#import <gui.h>

@interface NodeItem: CompositeItem <NodeItem>
{
  int x, y;
  const char *item;
  const char *text;
  const char *string;
  const char *font;
}

- setX: (int)x Y: (int)y;
- (int)getX;
- (int)getY;
- setString: (const char *)string;
- setFont: (const char *)the_font;
- setColor: (const char *)aColor;
- setBorderColor: (const char *)aColor;
- setBorderWidth: (int)aVal;
- createBindings;
- createText;
- createPaddedText;
@end

