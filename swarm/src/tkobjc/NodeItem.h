// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

#import <tkobjc/CompositeItem.h>

@interface NodeItem: CompositeItem
{
  int x,y;
  const char *text;
  const char *item;
  const char *string;
  const char *font;
}

- setX: (int)x Y: (int)y;
- (int)getX;
- (int)getY;
- setString: (const char *)string;
- setFont: (const char *) the_font;
- setString: (const char *) the_text usingFont: (const char*) the_font;
- setColor: (const char *)aColor;
- setBorderColor: (const char *)aColor;
- setBorderWidth: (int)aVal;
- createBindings;
@end

