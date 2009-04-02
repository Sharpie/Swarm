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

//S: tools for visualizing objects in various spaces

//D: The Swarm Space library is the beginnings of a library to assist in
//D: building environments for interacting agents. In general, environments
//D: can be just as varied as the agents themselves (in one view, the
//D: environment itself is simply another agent). However, many simulations
//D: have similar types of environments that can be helpfully supported by
//D: generic code.

//D: The current space library only addresses simple kinds of discretized
//D: 2d space. Improvement is planned in the future: see the todo list for
//D: ideas. Briefly, coordinates need to be elevated to the status of
//D: objects, which should hopefully allow spaces of different scales and
//D: boundary conditions to interact through a common reference system. In
//D: addition, other types of spaces are desired: continuous coordinates,
//D: other dimensions, arbitrary graphs, etc.

#import <Swarm/objectbase.h>
#import <Swarm/swarmconfig.h>
#ifndef DISABLE_GUI
#if !defined(GNUSTEP) && !defined(SWARM_OSX)
#import <Swarm/gui.h> // Raster, Colormap
#endif
#endif

@protocol GridData
//S: Methods used by Value2dDisplay and Object2dDisplay for display

//D: Methods required by widgets that display grids. User defined
//D: space objects must adopt this or any implementor of it
//D: in order to be accepted as data providers
//D: in the setDiscrete2dToDisplay method of  Value2dDisplay and
//D: Object2dDisplay  objects. User spaces must also define the
//D: macro discrete2dSiteAt(), versions of which can be
//D: found in Grid2d.h or Discrete2d.h.
USING
//M: Get the size of the lattice in the X dimension.
- (unsigned)getSizeX;

//M: Get the size of the lattice in the Y dimension.
- (unsigned)getSizeY;

//M: Returns the lattice pointer - use this for fast access.
- (id *)getLattice;

- (long *)getOffsets;

//M: Return the pointer stored at (x,y).
- getObjectAtX: (unsigned)x Y: (unsigned)y;

//M: Return the integer stored at (x,y). 
- (long)getValueAtX: (unsigned)x Y: (unsigned)y;
@end   

@protocol Discrete2d <SwarmObject, GridData, CREATABLE>
//S: Root class of all 2d discrete spaces.

//D: A Discrete2d is basically a 2d array of ids.  
//D: Subclasses add particular space semantics onto this.
//D: Currently Discrete2d grids are accessed by integer pairs
//D: of X and Y coordinates. 

//E: Discrete2d instances can now serialize themselves (without needing
//E: additional classes such as Int2dFiler).
//E:
//E: The result of value (shallow) Lisp serialization of a 4x3 Discrete2d 
//E: consisting of long values might be:
//E: 
//E: (list
//E:   (cons 'myDiscrete2d
//E:     (make-instance 'Discrete2d 
//E:      #:xsize 4 #:ysize 3 #:lattice 
//E:       (parse 
//E:        #2((1000 1000 1000 1000)
//E:           (1000 1000 1000 1000)
//E:           (1000 1000 10 1000))))))
//E:
//E: For object (deep) Lisp serialization of the same 4x3 lattice with 
//E: identical instances of MyClass at each point (except at (2,2) which
//E: has an instance of MyClassOther) would look like:
//E:
//E: (list
//E:   (cons 'myDiscrete2d
//E:     (make-instance 'Discrete2d 
//E:       #:xsize 4 #:ysize 3 #:lattice 
//E:        (parse
//E:        (cons '(0 . 0)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(0 . 1)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(0 . 2)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(1 . 0)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(1 . 1)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(1 . 2)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(2 . 0)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(2 . 1)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(2 . 2)
//E:         (make-instance 'MyClassOther #:strVal "Other World"))
//E:        (cons '(3 . 0)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(3 . 1)
//E:         (make-instance 'MyClass #:strVal "Hello World"))
//E:        (cons '(3 . 2)
//E:         (make-instance 'MyClass #:strVal "Hello World"))))))

CREATING
//M: Convenience constructor for Discrete2d lattice
+ create: (id <Zone>)aZone setSizeX: (unsigned)x Y: (unsigned)y;

//M: Set the world size.
- setSizeX: (unsigned)x Y: (unsigned)y;

//M: Allocate memory for the lattice. 
- (id *)allocLattice;

//M: Given an array size, compute the offsets array that
//M: caches the multiplication by ysize. See the discrete2dSiteAt macro. 
- makeOffsets;


SETTING
- setLattice: (id *)lattice;

//M: When objectFlag is true, indicates that this lattice is intended only for objects.
- (void)setObjectFlag: (BOOL)objectFlag;
USING
//M: Put the given pointer to (x,y) overwriting whatever was there.
- putObject: anObject atX: (unsigned)x Y: (unsigned)y;

//M: Put the given integer to (x,y) overwriting whatever was there.
- putValue: (long)v atX: (unsigned)x Y: (unsigned)y;

//M: Directly fills the lattice with a value.
- fastFillWithValue: (long)aValue;

//M: Directly fills the lattice with an object.
- fastFillWithObject: anObj;

//M: Fills the space using putValue.
- fillWithValue: (long)aValue;

//M: Fills the space using putObject.
- fillWithObject: anObj;

//M: This method reads a PGM formatted file and pipes the data into
//M: a Discrete2d object. 
- (int)setDiscrete2d: (id <Discrete2d>)a toFile: (const char *)filename;

//M: This method copies the data in one Discrete2d object to
//M: another Discrete2d object. It assumes that both objects already exist.
- copyDiscrete2d: (id <Discrete2d>)a toDiscrete2d: (id <Discrete2d>)b;

@end

@protocol DblBuffer2d <Discrete2d, CREATABLE>
//S: A double buffered space.

//D: DblBuffer2d augments Discrete2d to provide a form of double buffered
//D: space. Two lattices are maintained: lattice (the current state), and
//D: newLattice (the future state). All reads take place from lattice, all
//D: writes take place to newLattice. newLattice is copied to lattice when
//D: updateLattice is called.  DblBuffer2d can be used to implement one
//D: model of concurrent action, like in Ca2ds.  NOTE: be very careful if
//D: you're using low-level macro access to the world, in particular be
//D: sure that you preserve the write semantics on the newLattice.

CREATING

USING
//M: Return a pointer to the newLattice buffer.
- (id *)getNewLattice;

//M: Copy newLattice to lattice, in effect updating the lattice. 
- updateLattice;

#ifndef IDL
/* Disabled to avoid redeclaration with IDL */
//M: Overridden so writes happen to newLattice.
- putObject: anObject atX: (unsigned)x Y: (unsigned)y;

//M: Overridden so writes happen to newLattice.
- putValue: (long)v atX: (unsigned)x Y: (unsigned)y;
#endif
@end

@protocol Ca2d <DblBuffer2d>
//S: Defines abstract protocol for cellular automata.

//D: Inherits from DblBuffer2d, defines abstract protocol
//D: for cellular automata. 

CREATING
//M: Record the number of states the CA understands.
- setNumStates: (unsigned)n;

//M: Use this to set up your CA to a default initial state.
//M: Unimplemented in Ca2d; subclass this to set up initial state of lattice.
- initializeLattice;

USING
//M: One iteration of the CA rule.
//M: Unimplemented in Ca2d; subclass this to implement your CA rule.
- stepRule;
@end

#ifndef DISABLE_GUI
#ifndef GNUSTEP
#ifndef SWARM_OSX
@protocol Value2dDisplay <SwarmObject, CREATABLE>
//S: Value2dDisplay displays 2d arrays of values.

//D: Value2dDisplay helps display 2d arrays of values.
//D: Value2dDisplay goes through a given Discrete2d array,
//D: turn states into colours, and draws them into a Raster widget. 

CREATING
//M: Convenience constructor for Value2dDisplay
+ create: (id <Zone>)aZone setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c setDiscrete2dToDisplay: (id <GridData>)d;

//M: Set the display widget and the colourmap to use to draw the value array. 
- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c;

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
#endif
#endif
#endif

@protocol ConwayLife2d <Ca2d, CREATABLE>
//S: Classic 2d Conway's Life CA.

//D: Classic 2d Conway's Life CA.
CREATING

#ifndef IDL
//M: Initialize lattice to random 1/3 in state 1. 
- initializeLattice;
#endif

USING
#ifndef IDL
//M: Run Conway's Life rule (simpleminded version). 
- stepRule;
#endif
@end

@protocol Diffuse2d <Ca2d, CREATABLE>
//S: 2d difussion with evaporation.

//D: Discrete 2nd order approximation to 2d diffusion with evaporation. 
//D: Math is done in integers on the range [0,0x7fff]. 

CREATING
//M: Convenience constructor for Diffuse2d
+ create: (id <Zone>)aZone setSizeX: (unsigned)x Y: (unsigned)y setDiffusionConstant: (double)d setEvaporationRate: (double)e;

#ifndef IDL
//M: Initialize world to 0.
- initializeLattice;
#endif

SETTING

//M: Set the diffusion constant. Values over 1.0 might not be valid.
- setDiffusionConstant: (double)d;

//M: Set the evaporation rate. Values over 1.0 don't make much sense
- setEvaporationRate: (double)e;

USING
#ifndef IDL
//M: Run discrete approximation to diffusion. Roughly, it's 
//M: newHeat = evapRate * (self + diffuseConstant*(nbdavg - self)) 
//M: where nbdavg is the weighted average of the 8 neighbours. 
- stepRule;
#endif
@end

@protocol Grid2d <Discrete2d, CREATABLE>
//S: A 2d container class for agents.

//D: Grid2d is a generic container class to represent agent position on
//D: a 2d lattice.
//D: It gets most of its behaviour from Discrete2d, adding extra code to check
//D: that you don't overwrite things by accident. Grid2d is pretty primitive:
//D: only one object can be stored at a site, no boundary conditions are
//D: implied, etc.

CREATING

USING
#ifndef IDL
//M: Replaces the Discrete2d method.
//M: First check to see if it should do overwrite warnings, and if so
//M: if you're going to overwrite: if both conditions are true,
//M: print out a warning message.
//M: Regardless of the check, it writes the new object in. 
- putObject: anObject atX: (unsigned)x Y: (unsigned)y;
#endif

//M: If set to true, then if you try to store something at a site that
//M: doesn't have 0x0 there, a warning will be generated. 
- setOverwriteWarnings: (BOOL)b;
@end

#ifndef DISABLE_GUI
#ifndef GNUSTEP
#ifndef SWARM_OSX
@protocol Object2dDisplay <SwarmObject, CREATABLE>
//S: Object2dDisplay displays 2d arrays of objects.

//D: Object2dDisplay helps display 2d arrays of objects. 
//D: Create a Object2dDisplay, give it a Raster widget to draw on,
//D: a Discrete2d, a message to call on each object, and (optionally)
//D: a collection of objects and it will dispatch the message to all
//D: objects with the Raster widget as an argument. In addition,
//D: Object2dDisplay can help you make probees. 

CREATING
//M: Convenience constructor for Object2dDisplay
+ create: (id <Zone>)aZone setDisplayWidget: (id <Raster>)r setDiscrete2dToDisplay: (id <GridData>)c setDisplayMessage: (SEL)s;

//M: Set the display widget to use for drawing.
- setDisplayWidget: (id <Raster>)r;

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
#endif
#endif
#endif

@protocol Int2dFiler <SwarmObject, CREATABLE>
//S: Saves the state of a Discrete2d object [DEPRECATED].

//D: The Int2dFiler class is used to save the state of any Discrete2d
//D: object (or a subclass thereof) to a specified file.  

//D: Use of this protocol is deprecated, the ability to write the state 
//D: of a Discrete2d instance to disk (serialize) is now encoded directly 
//D: to the Discrete2d class, via the lisp and HDF5  archiver features.

USING
//M: Set the target space to be filled.
//M: This message can be used more than once, but often it is useful
//M: to keep one Int2dFiler per space
//M: (e.g. when the space is saved multiple times). 
- setDiscrete2dToFile: (id <Discrete2d>)sSpace;

//M: This message is optional. It is used when the target Discrete2d
//M: contains objects.  By sending each object the message specified by
//M: the selector, the Int2dFiler is able to get from the object an
//M: integer representing its state, which it then writes to the file. 
- setValueMessage: (SEL)aSelector;

//M: This message is optional.  It is used when the target Discrete2d
//M: contains objects.  If a particular location in the space has no
//M: resident object, the argument of this message is the value which
//M: gets writtent to the file. The default background value is 0. 
- setBackground: (int)aValue;

//M: When the Int2dFiler receives this message, it opens a file called
//M: fileName, stores the state of a pre-specified space into it, and
//M: then closes the file. 
- fileTo: (const char *)aFileName;
@end

@class Discrete2d;
@class DblBuffer2d;
@class Ca2d;
@class Value2dDisplay;
@class ConwayLife2d;
@class Diffuse2d;
@class Grid2d;
@class Object2dDisplay;
@class Int2dFiler;
