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

// Objective C interface for arbitrary Tk Widgets

#import <defobj.h>
#import <defobj/Create.h>
#import <gui.h>

@interface Widget: CreateDrop <Widget>
{
  Widget *parent;
  const char *widgetName;

  id destroyNotificationTarget;
  SEL destroyNotificationMethod;
  BOOL destroyedFlag;
  BOOL shellFrameFlag;
}

// creation time messages
- setParent: p;                                   // set parent widget
- createEnd;					  // finalize creation
+ createParent: p;                                // convenience interface

- (const char *)getWidgetName;			  // return the widget name
- getParent;                                      // return the parent
- getTopLevel;	                                  // return the top parent

- (const char *)getWindowGeometry;		  // get geometry as a string
- (unsigned)getWidth;				  // get geometry values
- (unsigned)getHeight;
- (int)getX;
- (int)getY;

- setWidth: (unsigned)w Height: (unsigned)h;	  // set size
- setWidth: (unsigned)w;
- setHeight: (unsigned)h;
- (void)setWindowGeometry: (const char *)s;       // set geometry as a string
- setX: (int)x Y: (int)y;		          // set window position.

- (void)setWindowTitle: (const char *)s;	  // window manager title

- (void)pack;					  // display the widget
- (void)packWith: (const char *)c;		  // display, args.
- (void)unpack;					  // unmap the widget

- (void)packToRight: widget;
- (void)packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- (void)packFillLeft: (BOOL)expandFlag;
- (void)packFill;
- (void)packForgetAndExpand;

- (void)setActiveFlag: (BOOL)activeFlag;

- setWidgetNameFromParent: parent;
- setWidgetNameFromParentName: (const char *)parentWidgetName;
- (const char *)makeWidgetNameFor: widget;

- (void)enableDestroyNotification: notificationTarget
               notificationMethod: (SEL)destroyNotificationMethod;
- (void)disableDestroyNotification;
- (BOOL)getDestroyedFlag;

- (void)drop;
@end
