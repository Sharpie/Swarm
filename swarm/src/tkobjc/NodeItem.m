// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/NodeItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>
#import <tkobjc/common.h>
#import <defobj/defalloc.h> // getZone
#include <misc.h> // stpcpy

static void
createButton3Binding (const char *canvasName,
                      const char *item,
                      const char *objectName)
{
  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
                  canvasName,
                  item,
                  objectName];
}

static void
createButton1Binding (const char *canvasName,
                      const char *item)
{
  [globalTkInterp eval: "%s bind %s <Button-1> {set curX %s; set curY %s}",
                  canvasName,
                  item,
                  "%x", "%y"];
}

static void
createButton1MotionBinding (const char *canvasName,
                            const char *item,
                            const char *objectName)
{
  [globalTkInterp eval: "%s bind %s <B1-Motion> {"
                  "%s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY];"
                  "set curX %s; set curY %s}",
                  canvasName,
                  item,
                  objectName,
                  "%x", "%y", "%x", "%y"];
}

@implementation NodeItem

PHASE(Creating)

- (void)createBindings
{
  const char *canvasName = [canvas getWidgetName];
  const char *objectName = [self getObjectName];

  createButton3Binding (canvasName, item, objectName);
  createButton3Binding (canvasName, text, objectName);

  createButton1Binding (canvasName, item);
  createButton1Binding (canvasName, text);
  
  createButton1MotionBinding (canvasName, item, objectName);
  createButton1MotionBinding (canvasName, text, objectName);
}

- setX: (int)the_x Y: (int)the_y
{
  x = the_x;
  y = the_y;

  return self;
}

- setString: (const char *)the_text
{
  string = STRDUP (the_text);
  return self;
}

- setFont: (const char *)the_font
{
  font = STRDUP (the_font);
  return self;
}


PHASE(Using)

- (void)setColor: (const char *)aColor
{
  [globalTkInterp eval: "%s itemconfigure %s -fill %s",
                  [canvas getWidgetName], item, aColor];  
}

- (void)setBorderColor: (const char *)aColor
{
  [globalTkInterp eval: "%s itemconfigure %s -outline %s",
                  [canvas getWidgetName], item, aColor];  
}

- (void)setBorderWidth: (int)aVal
{
  [globalTkInterp eval: "%s itemconfigure %s -width %d",
                  [canvas getWidgetName],
                  item,
                  aVal];
}

- (int)getX
{
  return x;
}

- (int)getY
{
  return y;
}

- (void)moveX: (long)the_x Y: (long)the_y
{
  const char *canvasName = [canvas getWidgetName];
  x += the_x;
  y += the_y;
  
  [globalTkInterp eval: "%s move %s %ld %ld; %s move %s %ld %ld",
                  canvasName, text, the_x, the_y,
                  canvasName, item, the_x, the_y];
}

- (void)resetString: (const char *)newString
{
  if (string)
    FREEBLOCK (string);
  
  string = STRDUP (newString);
  [globalTkInterp eval: "%s itemconfigure %s -text {%s}",
                  [canvas getWidgetName], text, string];  
}

- (void)createText
{
  text = tkobjc_createText (getZone (self), canvas, x, y, string, font, YES);
}

- (void)createPaddedText
{
  char stringpad[strlen (string) + 2], *ptr;
  
  ptr = stpcpy (stringpad, string);
  stpcpy (ptr, "xx");
  
  // font and size independence means I have to make a fake label first...
  text = tkobjc_createText (getZone (self), 
                            canvas, x, y, stringpad, font, YES);
}

- (void)drop
{
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName], text];  
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName], item];  
  FREEBLOCK (text);
  FREEBLOCK (item);
  if (font)
    FREEBLOCK (font);
  if (string)
    FREEBLOCK (string);
  [super drop];
}
 
@end
  
