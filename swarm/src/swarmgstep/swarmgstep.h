// Swarm library. Copyright © 1996-2004 Swarm Development Group.
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

//S: Classes to support GNUstep integration

#import <Swarm/objectbase.h>
#import <Swarm/space.h>

@protocol Object2dDisplay <SwarmObject, CREATABLE>
//S: Object2dDisplay displays 2d arrays of objects.

//D: Object2dDisplay helps display 2d arrays of objects. 

CREATING
//M: Convenience constructor for Object2dDisplay
+ create: (id <Zone>)aZone setDisplayWidget: (id)r setDiscrete2dToDisplay: (id <GridData>)c setDisplayMessage: (SEL)s;

//M: Set the display widget to use for drawing.
- setDisplayWidget: (id)r;

//M: Set the 2d array to draw.
- setDiscrete2dToDisplay: (id <GridData>)c;

//M: Set the message to be sent to each object in the grid to make it
//M: draw itself. 
- setDisplayMessage: (SEL)s;

USING
//M: Set a collection of objects to be displayed. 
//M: If this is not given, then Object2dDisplay loops through the 2d
//M: grid sending draw messages to all objects it finds there. 
//M: Giving an explicit collection of objects to draw is more efficient
//M: if your grid is sparsely populated. 
- setObjectCollection: objects;

//M: Draw all objects in the array (or optionally, the collection)
//M: on the raster widget. All that happens here is the display message
//M: is sent to each object - it is the object's responsibility to
//M: render itself. 
- display;

//M: Make a probe for an object at a specific point.
- makeProbeAtX: (unsigned)x Y: (unsigned)y;
@end

@protocol Value2dDisplay <SwarmObject, CREATABLE>
//S: Value2dDisplay displays 2d arrays of values.

//D: Value2dDisplay helps display 2d arrays of values.

CREATING
//M: Convenience constructor for Value2dDisplay
+ create: (id <Zone>)aZone setDisplayWidget: (id)r colormap: (id)c setDiscrete2dToDisplay: (id <GridData>)d;

//M: Set the display widget and the colourmap to use to draw the value array. 
- setDisplayWidget: (id)r colormap: (id)c;

//M: Set which array to draw. 
- setDiscrete2dToDisplay: (id <GridData>)c;

USING
//M: Linear transform of states to colours for drawing. 
//M: color = state / m + c 
//M: If not set, assume m == 1 and c == 0. 
- setDisplayMappingM: (int)m C: (int)c;

//M: Draw the array on the given widget.
//M: Note that you still have to tell the widget to draw itself afterwards.
//M: The code for display uses the fast macro access in Discrete2d on
//M: the cached return value from getLattice.
//M: It also caches the drawPointX:Y: method lookup on the display widget -
//M: this is a nice trick that you might want to look at. 
- display;
@end

@class Object2dDisplay;
@class Value2dDisplay;
