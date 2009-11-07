// Swarm library. Copyright (c) 1996-2009 Swarm Development Group.
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

//S: Classes to support OpenStep integration

#import <Swarm/objectbase.h>
#import <Swarm/space.h>

@protocol Object2dDisplay <SwarmObject, CREATABLE>
//S: Object2dDisplay displays 2d arrays of objects.

//D: Object2dDisplay helps display 2d arrays of objects. 

CREATING
//M: Convenience constructors for Object2dDisplay
+ create: aZone withDiscrete2dToDisplay: (id <GridData>)c;
+ create: aZone withDiscrete2dToDisplay: (id <GridData>)c andDisplayInvocation: (NSInvocation *)anInvocation;

//M: Set the 2d array to draw.
- setDiscrete2dToDisplay: (id <GridData>)c;

//M: Set the message to be sent to each object in the grid to make it
//M: draw itself.  If no invocation is set then used OpenStep NSView
//M: default of -drawRect:
- setDisplayInvocation: (NSInvocation *)anInvocation;

USING
//M: Draw all objects in the array
//M: on an internal image. All that happens here is the display message
//M: is sent to each object - it is the object's responsibility to
//M: render itself. 
- (void)updateDisplay;

//M: Get the image which all the objects have displayed themselves on.
//M: This is used by graphical classes like Raster to composite the
//M: image into a graphical view.
- (NSImage *)image;

//M: Make a probe for an object at a specific point.
- (void)makeProbeAtX: (unsigned)x Y: (unsigned)y;
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

//M: Get the image with the 2d array values displayed on it.
//M: This is used by graphical classes like Raster to composite the
//M: image into a graphical view.
- (NSImage *)image;

//M: Draw the array on an internal image.
//M: This essentially makes a snapshot of the state of the 2d array
//M: that can be used by graphical classes like Raster to composite
//M: into graphical views without requiring any access to the
//M: underlying 2d array.
- (void)updateDisplay;
@end

@class Object2dDisplay;
@class Value2dDisplay;
