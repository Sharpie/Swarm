// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/TextItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>
#include <misc.h> // strdup

@implementation TextItem

PHASE(Creating)

+ createBegin: aZone
{
  TextItem *obj = [super createBegin: aZone];
  
  obj->centerFlag = YES;
  
  return obj;
}

- setX: (int)the_x Y: (int)the_y
{
  x = the_x;
  y = the_y;

  return self;
}

- setText: (const char *)the_text
{
  text = the_text;

  return self;
}

- setFont: (const char *)the_font
{
  font = the_font;

  return self;
}

- setCenterFlag: (BOOL)theCenterFlag
{
  centerFlag = theCenterFlag;

  return self;
}
 
- createItem
{
  item = tkobjc_createText (canvas, x, y, text, font, centerFlag);

  return self;
}

PHASE(Using)

@end
