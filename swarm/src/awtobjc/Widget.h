// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/JavaObject.h>

@interface Widget: JavaObject
{
  Widget *parent;
  const char *widgetName;

  jmethodID _getBounds, _setSize, _setTitle;
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
- (int)getPositionX;
- (int)getPositionY;

- setWidth: (unsigned)w Height: (unsigned)h;	  // set size
- setWindowGeometry: (const char *)s;		  // set geometry as a string
- setPositionX: (int)x Y: (int)y;		  // set window position.

- setWindowTitle: (const char *)s;		  // window manager title

- pack;						  // display the widget
- packWith: (const char *)c;			  // display, args.
- unpack;					  // unmap the widget

- setActiveFlag: (BOOL)activeFlag;
- setWidgetNameFromParent: parent;
- setWidgetNameFromParentName: (const char *)parentWidgetName;
- (const char *)makeWidgetNameFor: widget; 
- packFillLeft: (BOOL)expandFlag;
- packFill;

@end
