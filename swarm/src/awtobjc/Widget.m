// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc.h>
#import <javaobjc/common.h>

@implementation Widget

- setParent: (Widget *)p
{
  if (parent == 0)
    {
      parent = p;
      return self;
    }
  else
    [InvalidCombination raiseEvent: "It is an error to reset a Widget's parent\n"];
  return nil;
}

- createEnd
{
  [super createEnd];
  if (parent == nil)
    {				  // no parent, make a frame
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

// this is the name of the Tk widget eg: .foo.bar
- (const char *)getWidgetName
{
  return widgetName;
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
  jobject bounds;
  static char buf[1024]; // meps!  meps!
  
  if (_getBounds == 0)
    _getBounds = [self findMethod: "getBounds" signature: "()Ljava/awt/Rectangle;"];
  bounds = [self callObjectMethod: _getBounds];
  
  sprintf (buf, "%dx%d+%d+%d", 
           getIntField (jniEnv, bounds, "width"),
           getIntField (jniEnv, bounds, "height"),
           getIntField (jniEnv, bounds, "x"),
           getIntField (jniEnv, bounds, "y"));
  
  return buf;
}

// ugh, repeated code here for sscanf. Too bad C doesn't allow multiple
// return values.
- (unsigned)getWidth
{
  unsigned w, h;
  int x, y;
  if (sscanf([self getWindowGeometry], "%dx%d+%d+%d", &w, &h, &x, &y) != 4)
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
  return self;
}

- setWidth: (unsigned)w Height: (unsigned)h
{
  fprintf(stderr, "set size...\n");
  // get back the bounding rectangle
  if (_setSize == 0)
    _setSize = [self findMethod: "setSize" signature: "(II)V"];
  [self callVoidMethod: _setSize : w : h];
  return self;
}

- setPositionX: (int)x Y: (int)y
{
  char s[128];

  sprintf(s, "%+d%+d", x, y);
  return [self setWindowGeometry: s];
}


- setWindowTitle: (const char *)s
{
  // get back the bounding rectangle
  if (_setTitle == 0)
    _setTitle = [self findMethod: "setTitle" signature: "(Ljava/lang/String;)V"];
  if (_setTitle != 0) 
    [self callVoidMethod: _setTitle S: s];
  else
    abort ();
  return self;
}

- pack
{
  return self;
}

- packWith: (const char *)c
{
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
static const char *
makeWidgetName (const char *parentWidgetName, id newWidget)
{
  char *buf;
#ifdef LONGNAMES
  char *n;
  n = [newWidget getObjcName];  // my object name in Tclland
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

- packFillLeft: (BOOL)expandFlag
{
  printf ("packFillLeft: %d\n", expandFlag);
  return self;
}    

- setActiveFlag: (BOOL)theActiveFlag
{
  printf ("setActiveFlag: %d\n", theActiveFlag);
  return self;
}

- packFill
{
  printf ("packFill\n");
  return self;
}

@end
