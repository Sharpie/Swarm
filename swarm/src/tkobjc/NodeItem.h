// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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
- setString: (const char *)string;
- setFont: (const char *)font;

- (int)getX;
- (int)getY;
- (void)resetString: (const char *)string;
- (void)setColor: (const char *)aColor;
- (void)setBorderColor: (const char *)aColor;
- (void)setBorderWidth: (int)aVal;
- (void)createBindings;
- (void)createText;
- (void)createPaddedText;
@end

