// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/Frame.h>
#import <tkobjc/common.h>

@implementation Widget

PHASE(Creating)

- setParent: (Widget *)p
{
  if (parent == 0)
    {
      parent = p;
      return self;
    }
  else
    {
      [InvalidCombination raiseEvent:
                            "It is an error to reset a Widget's parent\n"];
      return nil;
    }
}  

- createEnd
{
  if (parent == nil)
    { 
      // no parent, make a frame
      Frame *defaultFrame;
      defaultFrame = [Frame create: [self getZone]];
      [self setParent: defaultFrame];
    }
  [self setWidgetNameFromParent: parent];
  
  return self;
}

// convenience interface for ease of setting.
+ createParent: (Widget *)p
{
  return [[[self createBegin: [p getZone]] setParent: p] createEnd];
}

// fill in the "parent" and "name" fields for a widget, based on algorithm.
// "name" is parent.w<objectName>, where <objectName> is the tclObjc name
// for self (long version) or just the pointer (short version)
static const char *
makeWidgetName (const char *parentWidgetName, id newWidget)
{
  char *buf;
#ifdef LONGNAMES
  char *n;
  n = [newWidget getObjectName];  // my object name in Tclland
#else
  char n[33];
  sprintf (n, "%p", newWidget); // my pointer (use %p?)
#endif			
  
  buf = [[newWidget getZone] alloc: strlen (parentWidgetName) + strlen (n) + 3];

  if (parentWidgetName[0] == '.' 
      && parentWidgetName[1] == 0)    // special case parent "."
    sprintf (buf, ".w%s", n);
  else
    sprintf (buf, "%s.w%s", parentWidgetName, n); // put in a "w" there.

  return buf;
}

- (const char *)makeWidgetNameFor: newWidget
{
  return makeWidgetName ([self getWidgetName], newWidget);
}

- setWidgetNameFromParent: theParent
{
  widgetName = makeWidgetName ([theParent getWidgetName], self);

  return self;
}

- setWidgetNameFromParentName: (const char *)theParentName
{
  widgetName = makeWidgetName (theParentName, self);

  return self;
}

PHASE(Using)

// this is the name of the Tk widget eg: .foo.bar
- (const char *)getWidgetName
{
  return widgetName;
}

- (Widget *)getParent
{
  return parent;
}

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

static int
get_geometry_element (id widget, unsigned offset)
{
  const char *p = [widget getWindowGeometry];
  int i;

  for (i = 0; i < offset; i++)
    {
      while (!(*p == '+' || *p == '-' || *p == 'x'))
        p++;
      p++;
    }
  return atoi (p);
}
        
- (unsigned)getWidth
{
  return get_geometry_element (self, 0);
}

- (unsigned)getHeight
{
  return get_geometry_element (self, 1);
}

- (int)getX
{
  return get_geometry_element (self, 2);
}

- (int)getY
{
  return get_geometry_element (self, 3);
}

- setWindowGeometry: (const char *)s
{
  [globalTkInterp eval: "wm geometry %s \"%s\"",
		  [[self getTopLevel] getWidgetName], s];

  return self;
}

- setWidth: (unsigned)w
{
  [globalTkInterp eval: "%s configure -width %u", widgetName, w];

  return self;
}

- setHeight: (unsigned)h
{
  [globalTkInterp eval: "%s configure -height %u", widgetName, h];

  return self;
}

- setWidth: (unsigned)w Height: (unsigned)h
{
  return [[self setWidth: w] setHeight: h];
}

- setX: (int)x Y: (int)y
{
  tkobjc_move (self, x, y);

  return self;
}


- setWindowTitle: (const char *)s
{
  [globalTkInterp eval: "wm title %s \"%s\"", 
		  [[self getTopLevel] getWidgetName], s];

  return self;
}

- pack
{
  tkobjc_pack (self);

  return self;
}

- packWith: (const char *)c
{
  [globalTkInterp eval: "pack %s %s;", widgetName, c];

  return self;
}

- packToRight: widget
{
  [globalTkInterp eval: "pack %s %s -side right",
                  [self getWidgetName],
                  [widget getWidgetName]];

  return self;
}

- packBeforeAndFillLeft: widget expand: (BOOL)expandFlag
{
  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand %d",
		  [self getWidgetName],
		  [widget getWidgetName],
                  (int)expandFlag];

  return self;
}

- packFillLeft: (BOOL)expandFlag
{
  [globalTkInterp eval: "pack %s -side left -fill both -expand %d",
		  [self getWidgetName],
                  (int)expandFlag];

  return self;
}

- packFill
{
  [globalTkInterp eval: "pack %s -fill both -expand 0",
		  [self getWidgetName]];

  return self;
}

- packForgetAndExpand
{
  [globalTkInterp eval: "pack forget %s",
                  [self getWidgetName]];
  [globalTkInterp eval:  "pack %s -expand true -fill both",
                  [self getWidgetName]];

  return self;
}

- unpack
{
  [NotImplemented raiseEvent];

  return self;
}

- setActiveFlag: (BOOL)activeFlag
{
  [globalTkInterp eval: "%s configure -state %s",
                  [self getWidgetName],
                  activeFlag ? "normal" : "disabled"];

  return self;
}

@end
