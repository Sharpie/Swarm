// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/NodeItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>
#import <tkobjc/common.h>
#include <misc.h> // stpcpy

@implementation NodeItem

- setX: (int)the_x Y: (int)the_y
{
  x = the_x;
  y = the_y;

  return self;
}

- setColor: (const char *)aColor
{
  [globalTkInterp eval: "%s itemconfigure %s -fill %s",
                  [canvas getWidgetName], item, aColor];  

  return self;
}

- setBorderColor: (const char *)aColor
{
  [globalTkInterp eval: "%s itemconfigure %s -outline %s",
                  [canvas getWidgetName], item, aColor];  

  return self;
}

- setBorderWidth: (int)aVal
{
  [globalTkInterp eval: "%s itemconfigure %s -width %d",
                  [canvas getWidgetName],
                  item,
                  aVal];  

  return self;
}

- setString: (const char *)the_text
{
  string = the_text;

  return self;
}

- setFont: (const char *)the_font
{
  font = the_font;

  return self;
}

- createBindings
{
  const char *canvasName = [canvas getWidgetName];
  const char *objectName = [self getObjectName];

  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
                  canvasName, item, objectName];
  
  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
                  canvasName, text, objectName];
  
  [globalTkInterp eval: "%s bind %s <Button-1> {set curX %s; set curY %s}",
                  canvasName,
                  item,
                  "%x", "%y"];
  
  [globalTkInterp eval: "%s bind %s <Button-1> {set curX %s; set curY %s}",
                  canvasName,
                  text,
                  "%x", "%y"];
  
  [globalTkInterp eval: "%s bind %s <B1-Motion> {"
                  "%s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY];"
                  "set curX %s; set curY %s}",
                  canvasName,
                  item,
                  objectName,
                  "%x", "%y", "%x", "%y"];

  [globalTkInterp eval: "%s bind %s <B1-Motion> {"
                  "%s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY];"
                  "set curX %s; set curY %s}",
                  canvasName,
                  text,
                  objectName,
                  "%x", "%y", "%x", "%y"];
  return self;
}

- (int)getX
{
  return x;
}

- (int)getY
{
  return y;
}

- moveX: (long)the_x Y: (long)the_y
{
  x += the_x;
  y += the_y;
  
  [globalTkInterp eval: "%s move %s %ld %ld; %s move %s %ld %ld",
                  [canvas getWidgetName], text, the_x, the_y,
                  [canvas getWidgetName], item, the_x, the_y];
  
  return self;
}

- createText
{
  text = tkobjc_createText (canvas, x, y, string, font);

  return self;
}

- createPaddedText
{
  char stringpad[strlen (string) + 2], *ptr;
  
  ptr = stpcpy (stringpad, string);
  stpcpy (ptr, "xx");
  
  // font and size independence means I have to make a fake label first...
  text = tkobjc_createText (canvas, x, y, stringpad, font);

  return self;
}

- (void)drop
{
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName], text];  
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName], item];  
  [super drop];
}
 
@end
  
