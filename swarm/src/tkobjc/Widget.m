// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


// should Widget have an "eval" method that it can send to itself?
// varargs overhead.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#import <tkobjc/global.h>
#import <tclObjc.h>
#import <Tk.h>
#import <tkobjc/Widget.h>
#import <tkobjc/Frame.h>

@implementation Widget

- setParent: (Widget *)p
{
  if (parent == 0)
    {
      parent = p;
      return self;
    }
  else
    {
      [InvalidCombination raiseEvent: "It is an error to reset a Widget's parent\n"];
      return nil;
    }
}  

- createEnd
{
  const char *p;
  char *name;
  
  if (parent == nil)
    {				  // no parent, make a frame
      Frame * defaultFrame;
      defaultFrame = [Frame create: [self getZone]];
      [self setParent: defaultFrame];
    }
  [self makeNameFromParentName: [parent getWidgetName]];
  
  // make our own copy of tclObjc_objectToName (it uses a static buffer.)
  p = tclObjc_objectToName(self);
  name = [[self getZone] alloc: strlen(p) + 1];
  strcpy (name, p);
  objcName = name;
  
  return self;
}

// convenience interface for ease of setting.
+ createParent: (Widget *) p
{
  return [[[self createBegin: [p getZone]] setParent: p] createEnd];
}

// this is the name of the Tk widget eg: .foo.bar
- (const char *)getWidgetName
{
  return widgetName;
}

// this is the name of the objc command for ourselves eg: Widget@0x1ab0
- (const char *)getObjcName
{
  return objcName;
}

- (Widget *)getParent
{
  return parent;
}

// whee, recursion! The widget with no parent is the toplevel.
- (Widget *)getTopLevel
{
  if (parent == nil)
    return self;
  else
    return [parent getTopLevel];
}


- (const char *)getWindowGeometry
{
  [globalTkInterp eval: "wm geometry %s", [[self getTopLevel] getWidgetName]];
  return [globalTkInterp result];
}

// ugh, repeated code here for sscanf. Too bad C doesn't allow multiple
// return values.
- (unsigned)getWidth
{
  unsigned w, h;
  int x, y;
  if (sscanf ([self getWindowGeometry], "%dx%d+%d+%d", &w, &h, &x, &y) != 4)
    [WarningMessage raiseEvent: "Widget - invalid geometry"];
  return w;
}

- (unsigned)getHeight
{
  unsigned w, h;
  int x, y;
  if (sscanf([self getWindowGeometry], "%dx%d+%d+%d", &w, &h, &x, &y) != 4)
    [WarningMessage raiseEvent: "Widget - invalid geometry"];
  return h;
}

- (int)getPositionX
{
  unsigned w, h;
  int x, y;
  if (sscanf([self getWindowGeometry], "%dx%d+%d+%d", &w, &h, &x, &y) != 4)
    [WarningMessage raiseEvent: "Widget - invalid geometry"];
  return x;
}

- (int)getPositionY
{
  unsigned w, h;
  int x, y;
  if (sscanf([self getWindowGeometry], "%dx%d+%d+%d", &w, &h, &x, &y) != 4)
    [WarningMessage raiseEvent: "Widget - invalid geometry"];
  return y;
}

// this really shouldn't be used to set width/height.
- setWindowGeometry: (const char *)s
{
  [globalTkInterp eval: "wm geometry %s \"%s\"",
		  [[self getTopLevel] getWidgetName], s];
  return self;
}

- setWidth: (unsigned) w
{
  [globalTkInterp eval: "%s configure -width %u", widgetName, w];
  return self;
}

- setHeight: (unsigned) h
{
  [globalTkInterp eval: "%s configure -height %u", widgetName, h];
  return self;
}

- setWidth: (unsigned) w Height: (unsigned) h
{
  return [[self setWidth: w] setHeight: h];
}

- setPositionX: (int) x Y: (int) y
{
  char s[128];

  sprintf(s, "%+d%+d", x, y);
  return [self setWindowGeometry: s];
}


- setWindowTitle: (const char *)s
{
  [globalTkInterp eval: "wm title %s \"%s\"", 
		  [[self getTopLevel] getWidgetName], s];
  return self;
}

- pack
{
  [globalTkInterp eval: "pack %s -fill both -expand true;", widgetName];
  return self;
}

- packWith: (const char *) c
{
  [globalTkInterp eval: "pack %s %s;", widgetName, c];
  return self;
}

- unpack
{
  [NotImplemented raiseEvent];
  return self;
}

// fill in the "parent" and "name" fields for a widget, based on algorithm.
// "name" is parent.w<objectName>, where <objectName> is the tclObjc name
// for self (long version) or just the pointer (short version)
- makeNameFromParentName: (const char *)p
{
  char *buf;
#ifdef LONGNAMES
  char *n;
  n = [self getObjcName];			  // my object name in Tclland
#else
  char n[33];
  sprintf(n, "%p", self);		  // my pointer (use %p?)
#endif			
  
  buf = [[self getZone] alloc: strlen(p) + strlen(n) + 3];

  if (p[0] == '.' && p[1] == 0)			  // special case parent "."
    sprintf (buf, ".w%s", n);
  else
    sprintf (buf, "%s.w%s", p, n);	  // put in a "w" there.

  widgetName = buf;
  return self;
}

- (void)drop
{
  [[self getZone] free: (void *)objcName];
  [super drop];
}

@end
