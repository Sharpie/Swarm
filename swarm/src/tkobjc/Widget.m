// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#include "internal.h" // XEvent
#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/Frame.h>
#import <tkobjc/common.h>
#import <defobj/defalloc.h> // getZone

#include <swarmconfig.h> // PTRUINT
#include <misc.h> // atoi

@implementation Widget

PHASE(Creating)

- setParent: p
{
  if (parent == nil)
    {
      parent = p;
      return self;
    }
  else
    {
      raiseEvent (InvalidCombination,
                  "It is an error to reset a Widget's parent\n");
      return nil;
    }
}  

- createEnd
{
  if (parent == nil)
    { 
      [self setParent: [Frame create: getZone (self)]];
      shellFrameFlag = YES;
    }
  [self setWidgetNameFromParent: parent];
  
  return self;
}

// convenience interface for ease of setting.
+ createParent: p
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
  sprintf (n, PTRUINTFMT, (PTRUINT) newWidget); // my pointer (use %p?)
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

- getParent
{
  return parent;
}

- getTopLevel
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
  unsigned i;

  for (i = 0; i < offset; i++)
    {
      while (!(*p == '+' || *p == 'x'))
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

- (void)setWindowGeometry: (const char *)s
{
  [globalTkInterp eval: "wm geometry %s {%s}",
		  [[self getTopLevel] getWidgetName], s];
}

- setWidth: (unsigned)w
{
  [globalTkInterp eval: "%s configure -width %u",
		  [self getWidgetName],
		  w];
  return self;
}

- setHeight: (unsigned)h
{
  [globalTkInterp eval: "%s configure -height %u",
		  [self getWidgetName],
		  h];
  return self;
}

- setWidth: (unsigned)w Height: (unsigned)h
{
  if (parent)
    [globalTkInterp eval: "%s configure -width %u -height %u",
                    [self getWidgetName],
                    w, h];
  else
    {
      char buf[5 + 1 + 5 + 1];

      sprintf (buf, "%ux%u", w, h);
      [self setWindowGeometry: buf];
    }

  return self;
}

- setX: (int)x Y: (int)y
{
  tkobjc_move (self, x, y);

  return self;
}


- (void)setWindowTitle: (const char *)s
{
  [globalTkInterp eval: "wm title %s {%s}", 
                  [[self getTopLevel] getWidgetName], s];
}

- (void)pack
{
  tkobjc_pack (self);
}

- (void)packWith: (const char *)c
{
  [globalTkInterp eval: "pack %s %s;", widgetName, c];
}

- (void)packToRight: widget
{
  [globalTkInterp eval: "pack %s %s -side right",
                  [self getWidgetName],
                  [widget getWidgetName]];
}

- (void)packBeforeAndFillLeft: widget expand: (BOOL)expandFlag
{
  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand %d",
		  [self getWidgetName],
		  [widget getWidgetName],
                  (int) expandFlag];
}

- (void)packFillLeft: (BOOL)expandFlag
{
  [globalTkInterp eval: "pack %s -side left -fill both -expand %d",
		  [self getWidgetName],
                  (int) expandFlag];
}

- (void)packFill
{
  [globalTkInterp eval: "pack %s -fill both -expand 0",
		  [self getWidgetName]];
}

- (void)packForgetAndExpand
{
  [globalTkInterp eval: "pack forget %s",
                  [self getWidgetName]];
  [globalTkInterp eval:  "pack %s -expand true -fill both",
                  [self getWidgetName]];
}

- (void)unpack
{
  [NotImplemented raiseEvent];
}

- (void)setActiveFlag: (BOOL)activeFlag
{
  [globalTkInterp eval: "%s configure -state %s",
                  [self getWidgetName],
                  activeFlag ? "normal" : "disabled"];
}

- (void)drop
{
  [self disableDestroyNotification];
  
  if (!destroyedFlag)
    {
      if (parent == nil)
        Tk_DestroyWindow (tkobjc_nameToWindow ([self getWidgetName]));
      if (shellFrameFlag)
        [parent drop];
      [super drop];
    }
}

- (BOOL)getDestroyedFlag
{
  return destroyedFlag;
}

- _notifyTarget_
{
  destroyedFlag = YES;
  [destroyNotificationTarget perform: destroyNotificationMethod with: self];
  return self;
}

static void
structure_proc (ClientData clientdata, XEvent *eventptr)
{
  if (eventptr->type == DestroyNotify)
    [(id)clientdata _notifyTarget_];
}

- (void)enableDestroyNotification: theNotificationTarget
               notificationMethod: (SEL)theNotificationMethod
{
  if (theNotificationTarget)
    {
      tkobjc_createEventHandler (self, structure_proc);
      
      destroyNotificationTarget = theNotificationTarget;
      destroyNotificationMethod = theNotificationMethod;
    }
}

- (void)disableDestroyNotification
{
  if (destroyNotificationTarget != nil)
    {
      tkobjc_deleteEventHandler (self, structure_proc);
      destroyNotificationTarget = nil;
    }
}


@end
