// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Objective C interface for arbitrary Tk Widgets

#import <defobj.h>
#import <defobj/Create.h>
#import <gui.h>

@interface Widget: CreateDrop <_Widget>
{
  Widget *parent;
  const char *widgetName;
}

// creation time messages
- setParent: (Widget *)p;			  // set parent widget
- createEnd;					  // finalize creation
+ createParent: (Widget *)p;			  // convenience interface

- (const char *)getWidgetName;			  // return the widget name
- (Widget *)getParent;				  // return the parent
- (Widget *)getTopLevel;			  // return the top parent

- (const char *)getWindowGeometry;		  // get geometry as a string
- (unsigned)getWidth;				  // get geometry values
- (unsigned)getHeight;
- (int)getX;
- (int)getY;

- setWidth: (unsigned)w Height: (unsigned)h;	  // set size
- setWidth: (unsigned)w;
- setHeight: (unsigned)h;
- setWindowGeometry: (const char *)s;             // set geometry as a string
- setX: (int) x Y: (int) y;		  // set window position.

- setWindowTitle: (const char *)s;		  // window manager title

- pack;						  // display the widget
- packWith: (const char *)c;			  // display, args.
- unpack;					  // unmap the widget

- packToRight: widget;
- packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- packFillLeft: (BOOL)expandFlag;
- packFill;
- packForgetAndExpand;

- setActiveFlag: (BOOL)activeFlag;

- setWidgetNameFromParent: parent;
- setWidgetNameFromParentName: (const char *)parentWidgetName;
- (const char *)makeWidgetNameFor: widget;

@end
