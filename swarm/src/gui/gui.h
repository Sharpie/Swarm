// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <Swarm/objectbase.h>

//S: GUI interface for Swarm

//D: Tcl/Tk is a scripting language and graphical widget set. TkObjc is a
//D: library of wrapper classes around Tk and BLT widgets. It's purpose is
//D: to provide a simple graphical interface while hiding most Tk-specific
//D: code from library users. To create and use graphical widgets, the user
//D: merely needs to create and use objects. Many of the objects here are
//D: straightforward wrappings of Tk widgets, but some (ButtonPanel, for
//D: instance) are combinations of other widgets, and others (Raster) are
//D: novel code.

//D: TkObjc works with most configurations of Tcl, Tk and BLT.  It depends
//D: on the tclobjc package, the current version is 1.3 (available from
//D: Swarm authors). Very little of this code is library-version dependent,
//D: however, as most of it works by directly calling the Tk interpreter.

//D: The basic purpose of tkobjc is to package Tk functionality. Therefore,
//D: tkobjc's behaviour is similar to the Tk toolkit. For simple usage one
//D: should be able to get fairly far just by looking at this document, the
//D: header files, and the Swarm examples: more complicated graphical
//D: output will require the programmer have some familiarity with Tk.

@protocol Widget <Create, Drop>
//S: Widget base class.

//D: All graphical widgets inherit from the Widget base class. Widget
//D: defines most of the behaviour needed: Widgets are created by the user
//D: with a particular parent, and then "pack"ed in order to draw them on
//D: the screen.  All widgets have three essential things: a widget name
//D: used when running Tcl code, an Objective C name when sending messages
//D: from Tcl to those objects, and a parent.
CREATING
//M: When a widget is created it needs to be given a parent.  The parent
//M: widget will be the widget's containing window. If no parent is given
//M: (ie, a parent of nil), then a toplevel Frame will be allocated
//M: automatically
+ createParent: parent;

//M: Set the containing window of the widget.
- setParent: parent;

//M: Compute the widget name for a component widget.
- (const char *)makeWidgetNameFor: widget;

//M: Set the widget name using the parent as context.
- setWidgetNameFromParent: (id <Widget>)parent;

//M: Set the widget name using a hypothetical parent name.
- setWidgetNameFromParentName: (const char *)parentWidgetName;
USING
//M: Roughly, packing a widget makes it draw on the screen. The Tk packer
//M: allows complicated options to control widget layout. See documentation
//M: on Tk to learn more about packing details.
- (void)pack;

- (void)packFill;
- (void)packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- (void)packFillLeft: (BOOL)expandFlag;
- (void)packToRight : widget;
- (void)packForgetAndExpand;

//M: Enable or disable the widget.
- (void)setActiveFlag: (BOOL)activeFlag;

//M: Set the width of the widget.
- setWidth: (unsigned)width;

//M: Set the height of the widget.
- setHeight: (unsigned)height;

//M: Set the width and height of the widget.
- setWidth: (unsigned)width Height: (unsigned)height;

//M: Set the position of the widget.
- setX: (int)x Y: (int)y;

//M: Set the title on the widget.
- (void)setWindowTitle: (const char *)title;

//M: Get the containing window of the widget.
- getParent;

//M: Get top level frame
- getTopLevel;

//M: Get the widget name.
- (const char *)getWidgetName;

//M: Get the height of the widget.
- (unsigned)getHeight;

//M: Get the widget the widget.
- (unsigned)getWidth;

//M: Get the X position of the widget.
- (int)getX;

//M: Get the Y position of the widget.
- (int)getY;

- (const char *)getWindowGeometry;
- (void)setWindowGeometry: (const char *)s;

//M: Call a method if we are destroyed.
- (void)enableDestroyNotification: notificationTarget
               notificationMethod: (SEL)destroyNotificationMethod;

//M: Prevent calling the destroy notification method.
- (void)disableDestroyNotification;

- (BOOL)getDestroyedFlag;
@end


@protocol WindowGeometryRecord <Serialization, Create, Drop>
//S: A container for window geometry information.

//D: A container for window geometry information that implements
//D: archiving methods.
USING
//M: Set the window position.
- setX: (int)x Y: (int)y;

//M: Set the window size.
- setWidth: (unsigned)w Height: (unsigned)h;

//M: Get the flag that indicates if the size has been set.
- (BOOL)getSizeFlag;

//M: Get the flag that indicates if the position has been set.
- (BOOL)getPositionFlag;

//M: Get the window's horizontal size.
- (unsigned)getWidth;

//M: Get the window's vertical size.
- (unsigned)getHeight;

//M: Get the window's horizontal position.
- (int)getX;

//M: Get the window's vertical position.
- (int)getY;
@end

@protocol ArchivedGeometryWidget <Widget>
//S: Base class for widgets that archive geometry.

//D: Subclasses of this class inherit the ability to archive
//D: their window geometry.  This class also provides an interface
//D: to destroy notification.
CREATING
+ create: (id <Zone>)aZone setWindowGeometryRecordName: (const char *)name;

//M: Called to set a name for archiving.  recordName must not have spaces.
- setWindowGeometryRecordName: (const char *)recordName;

//M: Determines whether or not size is saved in addition to position.
- setSaveSizeFlag: (BOOL)saveSizeFlag;

- loadWindowGeometryRecord;
- registerAndLoad;
- updateSize;
USING
- (void)updateArchiver: archiver;
@end

@protocol Frame <ArchivedGeometryWidget, CREATABLE>
//S: Encapsulation of toplevels.

//D: Frames are boxes other widgets fit in. They correspond to the Tk
//D: "frame" and "toplevel" widgets. Frames can be new windows, or
//D: subwindows in an existing window. You only need to create frames
//D: yourself if building complicated composite widgets: by default, a
//D: frame will be built automatically for widgets without parents.
CREATING
//M: Determines the width of the border, if any.
- setBorderWidth: (int)width;

//M: Determines whether or not a frame has a border.
- setReliefFlag: (BOOL)reliefFlag;
USING
//M: Take the frame off screen.
- (void)withdraw;

//M: Deiconify the frame.
- (void)deiconify;
@end

@protocol Canvas <ArchivedGeometryWidget, CREATABLE>
//S: An interface to Tk canvas semantics.

//D: The Canvas widget allows display of a diverse range of graphical objects.
CREATING
USING
//M: Position a widget inside the canvas
- addWidget: widget X: (int)x Y: (int)y centerFlag: (BOOL)centerFlag;

//M: Remove a widget from the canvas.
- removeWidget: widget;

//M: Make sure the that the geometry is `reasonable'.
- checkGeometry: frame;
@end

@protocol ProbeCanvas <Canvas, CREATABLE>
//S: A canvas type for probe displays.

//D: ProbeCanvas is a Canvas that implements the general appearance and
//D: interface of a probe display.
CREATING
//M: Indicates the presence or absence of a horizontal scroll bar.
- setHorizontalScrollbarFlag: (BOOL)horizontalScrolbarFlag;
@end

@protocol GraphElement <Create, Drop, CREATABLE>
//S: Contains a set of related data for display.

//D: A GraphElement accumulates a related set of data for display,
//D: including attributes for the set.
CREATING
- setOwnerGraph: ownerGraph;

+ createOwnerGraph: ownerGraph;
USING
//M: Set the label for the element.
- (void)setLabel: (const char *)label;

//M: Set the color for the element.
- (void)setColor: (const char *)colorName;

//M: Set the dash style, 0 means solid.
- (void)setDashes: (int)dashesVal;

//M: Set the symbol for the element.
- (void)setSymbol: (const char *)symbol;

//M: Set the symbol size in pixels.
- (void)setSymbolSize: (unsigned)size;

//M: Add a new data point.
- (void)addX: (double)x Y: (double)y;

//M: Clear the data to be displayed.
- (void)resetData;

//M: Set the line width of the element.
- setWidth: (unsigned)w;
@end

@protocol Graph <ArchivedGeometryWidget, CREATABLE>
//S: A time series graph tool.

//D: A time series graph tool, based on BLT's graph widget.  Graph
//D: currently implements just a basic graph with multiple datasets, but
//D: should eventually support scaling and scrolling. For each Graph you
//D: create one or many GraphElements, one per dataset to
//D: plot. GraphElements can be configured for appearance, and data can be
//D: added to the element to draw.
CREATING
USING
//M: Set the title for the graph.
- setTitle: (const char *)title; // other modules are creating-phase
//M: Set the axis labels for the graph.
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
//M: Builds a new GraphElement to plot data with.
- (id <GraphElement>)createElement;
//M: Whether to autoscale every timestep or instead to jump scale.
- (void)setScaleModeX: (BOOL)xs Y: (BOOL)ys;
//M: Sets the X ranges for the graph.  Turns off autoscaling.
- (void)setRangesXMin: (double)minx Max:(double)maxx;
//M: Sets the Y ranges for the graph.  Turns off autoscaling.
- (void)setRangesYMin: (double)miny Max:(double)maxy;
//M: Sets the ranges for the graph.  Turns off autoscaling.
- (void)setRangesXMin: (double)minx Max:(double)maxx YMin: (double)miny Max: (double)maxy;
@end

@protocol Histogram <ArchivedGeometryWidget, CREATABLE>
//S: Histogram display tool.

//D: In Tk, this is based on BLT's barchart. 
//D: The number of bins is fixed at creation time, then the user hands
//D: the Histogram an array of datapoints (double or int) to display
//D: (or optionally an array of datapoints and locations where the bars
//D: should be drawn (specified as doubles).
CREATING
//M: Set the number of bins to use (bars to draw).
- setBinCount: (unsigned)n;
USING
//M: Set colors for the histogram bars. If not set, all are blue.
//M: Colors are arrays of strings (one per bin/bar) of color names.
- (void)setColors: (const char * const *)c count: (unsigned)count;

//M: Set labels for the histogram bars. If not set, they remain blank.
//M: Labels are arrays of strings, one per bin/bar.
- (void)setLabels: (const char * const *)l count: (unsigned)count;

//M: Set the title of the histogram.
- setTitle: (const char *)title; // other modules are creating-phase

//M: Set the width of the bars.
- (void)setBarWidth: (double)step;

//M: Set the X range and step size for the histogram.  
//M: Display three significant figures for the major-tick labels.
- (void)setXaxisMin: (double)min max: (double)max step: (double)step;

//M: Set the X range, step size, and number of major-tick-label significant figures for the histogram.
- (void)setXaxisMin: (double)min max: (double)max step: (double)step precision: (unsigned)precision;

//M: Set the axis labels.
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

//M: Set the text that describes a specified number of outliers.
- (void)setActiveOutlierText: (unsigned)outliers count: (unsigned)count;

//M: Hide the legend on the histogram.
- (void)hideLegend;

//M: Draw the (unsigned integer) data in the histogram.
- (void)drawHistogramWithInt: (unsigned *)points;

//M: Draw the (double) data in the histogram.
- (void)drawHistogramWithDouble: (double *)points;

//M: Draw the (unisgned integer) data in the histogram at particular offsets.
- (void)drawHistogramWithInt: (unsigned int *)points atLocations: (double *)locations;

//M: Draw the (double) data in the histogram at particular offsets.
- (void)drawHistogramWithDouble: (double *)points atLocations: (double *)locations;

- (void)setupZoomStack;
- (void)setupActiveOutlierMarker;
- (void)setupActiveItemInfo;
@end

@protocol Label <Widget, CREATABLE>
//S: A widget with text.

//D: A widget with text.
CREATING
USING
//M: Set the text to write in the label.
- setText: (const char *)text;
@end

@protocol ClassDisplayLabel <Label, CREATABLE>
//S: A label for displaying class names.

//D: This widget is used internally by ClassDisplayWidget.
CREATING
USING
@end

@protocol VarProbeLabel <Label, CREATABLE>
//S: A label for displaying variable names.

//D: This widget is used internally by VarProbeWidget.
CREATING
@end

@protocol CompleteProbeDisplayLabel <Label, CREATABLE>
//S: A class label used in a SimpleProbeDisplay.

//D: This widget is used internally by SimpleProbeDisplay.
//D: It is used to set up the mouse bindings to get a CompleteProbeDisplay,
//D: and to set up drag and drop.
CREATING
//M: Sets target widget which to report (e.g. probeDisplay).
- setTargetWidget: targetWidget;

//M: Sets the object that the probe display represents.
- setProbedObject: probedObject;
USING
@end

@protocol Button <Widget, CREATABLE>
//S: A button widget.

//D: A button widget that, when pressed, sends a method to a target object.
CREATING
USING
//M: Set the text for button.
- setText: (const char *)text;

//M: Set the target and selector for button.
- (void)setButtonTarget: target method: (SEL)method;
@end

@protocol ClassDisplayHideButton <Button, CREATABLE>
//S: The hide button used by a CompleteProbeDisplay.

//D: A button that handles the dismissal of class widgets on a
//D: ClassDisplayWidget (for CompleteProbeDisplay).
CREATING
- setSubWidget: subWidget;
- setUser: user;
- setOwner: owner;
USING
@end

@protocol SimpleProbeDisplayHideButton <Button, CREATABLE>
//S: The hide button used by a SimpleProbeDisplay.

//D: A button that handles the dismissal of a SimpleProbeDisplay.
CREATING
//M: The probe display in use.
- setProbeDisplay: probeDisplay;
@end

@protocol SuperButton <Button, CREATABLE>
//S: Request superclass in ClassDisplayWidget.

//D: A button used by ClassDisplayWidget to ask for superclass.
CREATING
- setSuperWidget: (id <Widget>)superWidget;
- setOwner: (id <Widget>)owner;
- setUser: user;
USING
@end

@protocol InputWidget <Widget>
//S: Abstract superclass for widgets that take input.

//D: InputWidgets get their input in one of two ways: by being readable, or
//D: by being linked to a C variable.
CREATING
USING
//M: Get the string value of the widget.
- (const char *)getValue;

//M: Set the string value of the widget. 
//M: This must be implemented by a subclass.
- (void)setValue: (const char *)v;

//M: Attach the widget value to an integer.
- (void)linkVariableInt: (int *)p;

//M: Attach the widget value to a double.
- (void)linkVariableDouble: (double *)p;

//M: Attach the widget value to a boolean.
- (void)linkVariableBoolean: (unsigned *)p;
@end

@protocol Entry <InputWidget, CREATABLE>
//S: Handles text-field input.

//D: Handles text-field input.
CREATING
USING
//M: This method aborts
- setHeight: (unsigned)h; // since this isn't possible with Tk, it will abort.
@end

@protocol MessageProbeEntry <Entry, CREATABLE>
//S: A widget for arguments to a message probe.

//D: An Entry widget for MessageProbe arguments.
CREATING
//M: Indicates whether the type of this entry is an id.
- setIdFlag: (BOOL)idFlag;

//M: Indicates the argument number.
- setArg: (int)arg;
USING
@end

@protocol VarProbeEntry <Entry, CREATABLE>
//S: A widget for variable probes.

//D: An Entry widget for VarProbes.
CREATING
//M: Indicates whether the entry is editable or not.
- setInteractiveFlag: (BOOL)interactiveFlag;

//M: Indicate the object that is using this widget.
- setOwner: owner;

//M: Set the variable probe associated with this widget.
- setVarProbe: varProbe;
USING
- getVarProbe;
@end

@protocol ButtonPanel <Frame, CREATABLE>
//S: Several buttons bound together in one frame.

//D: Several buttons bound together in one frame.
CREATING
USING
//M: Set a default target for use with addButtonName:method:.
- (void)setButtonTarget: target;

//M: Create a new button, and set both a target and method.
- (void)addButtonName: (const char *)name target: target method: (SEL)sel;

//M: Create a new button, and set the method, using the default target.
- (void)addButtonName: (const char *)name method: (SEL)sel;
@end

@protocol Form <Widget, CREATABLE>
//S: A set of Entry widgets bound together in one frame.

//D: A set of Entry widgets bound together in one frame.
CREATING
USING
//M: The width of all the Entry widgets.
- (void)setEntryWidth: (unsigned)ew;

//M: Add a boolean CheckButton widget.
- (void)addLineName: (const char *)n Boolean: (unsigned *)p;

//M: Add an Entry to get an integer.
- (void)addLineName: (const char *)n Int: (int *)p;

//M: Add an Entry to get a double.
- (void)addLineName: (const char *)n Double: (double *)p;
@end

@protocol CheckButton <InputWidget, CREATABLE>
//S: A check box on/off selection widget.

//D: A check box on/off selection widget.
CREATING
USING
//M: Get on/off status.
- (BOOL)getBoolValue;

//M: Turn the widget value and check button on or off.
- setBoolValue: (BOOL)v;
@end

typedef unsigned char Color; 
typedef unsigned long PixelValue;

@protocol Colormap <Create, CREATABLE>
//S: An class for creating a color palette for use with a Raster.

//D: Mechanism used to map numbers in the range [0, 255] to colour
//D: names. Create an XColormap, allocate colours in it, and pass it to a
//D: Raster widget for drawing.
CREATING
USING
//M: The current palette, per color-index.
- (PixelValue *)map;

//M: The pixel value for black.
- (PixelValue)black;

//M: The pixel value for white.
- (PixelValue)white;

//M: Add color index `c' to the color map, using a certain percent of 
//M: red, green, and blue.
- (BOOL)setColor: (Color)c ToRed: (double)r Green: (double)g Blue: (double)b;

//M: Add color index `c' looking up the color name in the color database.
- (BOOL)setColor: (Color)c ToName: (const char *)colorName;

//M: Add a color of a certain level of grey.
- (BOOL)setColor: (Color)c ToGrey: (double)g;

//M: Remove color at index `c' from the color map.
- (void)unsetColor: (Color)c;
@end

@class Raster;

@protocol Drawer
//S: The interface used by Raster to draw an arbitrary object.

//D: The interface used by Raster to draw an arbitrary object.
//D: Pixmap uses this.
USING
- (void)drawX: (int)x Y: (int)y;
@end

@protocol Raster <ArchivedGeometryWidget, CREATABLE>
//S: A two dimension color display class.

//D: 2 dimensional, colour pixel images. Raster is based on a Tk frame widget
//D: with our own code for fast display of images. You can draw coloured dots
//D: on a Raster, or generic Drawers. Raster widgets are
//D: double buffered - the pixels you draw are not actually put on the screen
//D: until drawSelf is called. In addition, Rasters handle mouse clicks.
CREATING
USING
//M: Set the palette for this raster.
- setColormap: (id <Colormap>)c;

//M: Draw a point at the given coordinates with the given color.
- (void)drawPointX: (int)x Y: (int)y Color: (Color)c;

//M: Draw the raster to the display.
- (void)drawSelf;

//M: Configure at mouse button to send a message to a given client object.
- (void)setButton: (int)n Client: c Message: (SEL)sel;

//M: Fill a rectangle of given geometry and color.
- (void)fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)color;

//M: Draw an ellipse of given geometry, pen width, and color.
- (void)ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
            Width: (unsigned)penWidth Color: (Color)c;

//M: Draw a line of given geometry, pen width, and color.
- (void)lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
         Width: (unsigned)penWidth Color: (Color)c;

//M: Draw a rectangle of given geometry, pen width, and color.
- (void)rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
              Width: (unsigned)penWidth Color: (Color)c;

//M: Draw an object at a given position.
- (void)draw: (id <Drawer>)drawer X: (int)x Y: (int)y;

//M: Erase the raster.
- (void)erase;
@end

@protocol ZoomRaster <Raster, CREATABLE>
//S: A zoomable Raster.

//D: ZoomRaster is a subclass of Raster that implements a zoomable image. It
//D: handles translation between logical coordinates and screen coordinates.
CREATING
USING
//M: Make the raster bigger.
- (void)increaseZoom;

//M: Make the raster smaller.
- (void)decreaseZoom;

//M: Get the current zoom factor.
- (unsigned)getZoomFactor;


//M: Special method for ZoomRasters. Like
//M: fillRectangleX0:Y0:X1:Y1:Color: in Raster, it will fill a rectangle
//M: of given geometry and color. This method makes sure that zooming the
//M: window does not change the logical position of the rectangle in
//M: relation to the logical coordinates.  In other words, if a rectangle
//M: includes point (10,10) at one zoom factors, then that same point is
//M: included for all zoom factors.

- (void)fillCenteredRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)color;


//M: Set the zoom factor.
- setZoomFactor: (unsigned)z;

//M: Reconfigures the ZoomRaster when the window is resized.
- (void)handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight;
@end

@protocol Pixmap <Drawer, Create, Drop, CREATABLE>
//S: A class for drawing color bitmaps on a Raster.

//D: A class for drawing color bitmaps on a Raster.  The bitmaps are
//D: stored in the Portable Network Graphics format.
CREATING
//M: Create a pixmap from a PNG file.
- setFile: (const char *)filename;

//M: Specify the directory to find the PNG file.
- setDirectory: (const char *)path;

//M: Create a pixmap from a widget, or from the root window if widget is nil.
- setWidget: (id <Widget>)widget;

//M: Specify whether or not window manager decorations for a widget
//M: should be included.
- setDecorationsFlag: (BOOL)decorationsFlag;
USING
//M: Set the raster that the pixmap will be shown on.
//M: It's used to augment raster the color palette as necessary.
- (void)setRaster: (id <Raster>)raster;

//M: Get the width of the bitmap in pixels.
- (unsigned)getWidth;

//M: Get the height of the bitmap in pixels.
- (unsigned)getHeight;

//M: Save the pixmap to a file.
- (void)save: (const char *)filename;
@end

@protocol CanvasAbstractItem <Create, Drop>
//S: An abstract class for items on a Canvas.

//D: CanvasAbstractItem is the root class of all items drawn on a Canvas.
CREATING
//M: Method to be implemented by subclass.
- (void)createItem;

//M: Method to be implemented by subclass.
- (void)createBindings;

//M: Designates the id of the Canvas in which this item resides.
- setCanvas: (id <Canvas>)canvas;
USING
//M: Designates the object to which this item refers.
- (void)setTargetId: target;

//M: Sets the message that will be sent upon a click on this item.
- (void)setClickSel: (SEL)sel;

//M: Sets the message that will effect the motion of the item on the canvas.
- (void)setMoveSel: (SEL)sel;

//M: Sets the message that will dictate what happens after the item is moved.
- (void)setPostMoveSel: (SEL)sel;

//M: Called when a mouse click occurs.
- (void)clicked;

//M: Method to be implemented by subclass.
- (void)initiateMoveX: (long)deltaX Y: (long)deltaY; 

//M: Return the canvas.
- (id <Canvas>)getCanvas;
@end

@protocol CanvasItem <CanvasAbstractItem, CREATABLE>
//S: An abstract superclass for simple Canvas items.

//D: An abstract superclass for non-composite Canvas items.
CREATING
USING
@end

@protocol CompositeItem <CanvasAbstractItem>
//S: A CanvasItem with several pieces.

//D: A CompositeItem is a CanvasItem that consists of several pieces.
//D: CompositeItem is an abstract superclass.
CREATING
USING
//M: Must be implemented by subclass.
- (void)moveX: (long)deltaX Y: (long)deltaY;
@end

@protocol NodeItem <CompositeItem, CREATABLE>
//S: A class for displaying a node on a Canvas.

//D: A class for displaying a node on a Canvas.
//D: A NodeItem has a position, a font, color, border color and width.
CREATING
//M: Set the label to put on the node.
- setString: (const char *)label;

//M: Set the font with which to draw the label.
- setFont: (const char *)font;

//M: Set the position of the node.
- setX: (int)x Y: (int)y;
USING
//M: Change the label on the string after the node is created.
- (void)resetString: (const char *)string;

//M: Get the x position of the node on the canvas.
- (int)getX;

//M: Get the y position of the node on the canvas.
- (int)getY;

//M: Set the color of the node.
- (void)setColor: (const char *)aColor;

//M: Set the border color of the node.
- (void)setBorderColor: (const char *)aColor;

//M: Set the width of the border.
- (void)setBorderWidth: (int)aVal;

//M: Create the text for the node.
- (void)createText;

//M: Create the space for the text for the node.
- (void)createPaddedText;
@end

@protocol LinkItem <CompositeItem, CREATABLE>
//S: A canvas item for displaying a link between two nodes.

//D: A CompositeCanvasItem for displaying a link between two NodeItems.
CREATING
//M: Designate the node that will be the source of the link.
- setFrom: (id <NodeItem>)from;

//M: Designate the node that will be the destination of the link.
- setTo: (id <NodeItem>)to;

//M: For disabling directed link items.
- setDirectedFlag: (BOOL)directedFlag;
USING
//M: Set the color of the link.
- (void)setColor: (const char *)aColor;

//M: Redraw the link (especially due to the motion of nodes).
- (void)update;
@end

@protocol ScheduleItem <CompositeItem, CREATABLE>
//S: A canvas item for displaying the time structure of a schedule.

//D: A CompositeCanvasItem for displaying the time structure of a schedule.
CREATING
//M: Set the schedule to be inspected.
- setSchedule: schedule;

//M: Set the horizontal spacing of a time step.
- setStep: (unsigned)step;

//M: Position the item on the canvas.
- setX: (int)x Y: (int)y;
USING
//M: Redraw widget with current values from Schedule.
- (void)update;

//M: Record the screen coordinates associated with a scheduling event.
- (void)at: (timeval_t)tval owner: owner widget: widget x: (int)sourceX y: (int)sourceY;

//M: Send visual message indicator from browser to some target.
- (void)trigger: widget X: (int)x Y: (int)y;
@end

@protocol OvalNodeItem <NodeItem, CREATABLE>
//S: A circular NodeItem.

//D: A NodeItem with a circular appearance.
CREATING
@end

@protocol RectangleNodeItem <NodeItem, CREATABLE>
//S: A rectangular NodeItem.

//D: A rectangular NodeItem.
CREATING
@end

@protocol TextItem <CanvasItem, CREATABLE>
//S: A CanvasItem that displays text.

//D: A CanvasItem that displays text.
CREATING
//M: Set the coordinate for the center of the text.
- setX: (int)x Y: (int)y;

//M: Set the text to display.
- setText: (const char *)text;

//M: Set the font with which to display the text.
- setFont: (const char *)font;

//M: Determine whether text is centered or not.
- setCenterFlag: (BOOL)centerFlag;
USING
@end

@protocol Circle <CanvasItem, CREATABLE>
//S: A CanvasItem that displays a circle.

//D: A CanvasItem that displays a circle.
CREATING
//M: Set the x, y coordinates for the center of the circle.
- setX: (int)x Y: (int)y;

//M: Set the radius of the circle.
- setRadius: (unsigned)r;
USING
@end

@protocol Rectangle <CanvasItem, CREATABLE>
//S: A CanvasItem that displays a rectangle.

//D: A CanvasItem that displays a rectangle.
CREATING
//M: Set the diagonal corner coordinates of the rectangle.
- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;
@end

@protocol Line <CanvasItem, CREATABLE>
//S: A CanvasItem that displays a line.

//D: A CanvasItem that displays a line.
CREATING
//M: Set the end points of the line.
- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;
@end

#if 0 /* def USE_JAVA -- disabled for the sake of protocol.el */
#define _GUI_MSG(str) printf ("GUI [%s,%d] %s\n",__FILE__,__LINE__,str)
#define GUI_BEEP() _GUI_MSG ("GUI_BEEP")
#define GUI_UPDATE() _GUI_MSG ("GUI_UPDATE")
#define GUI_UPDATE_IDLE_TASKS() _GUI_MSG ("GUI_UPDATE_IDLE_TASKS")
#define GUI_UPDATE_IDLE_TASKS_AND_HOLD() _GUI_MSG ("GUI_UPDATE_IDLE_TASKS_AND_HOLD")
#define GUI_RELEASE_AND_UPDATE()  _GUI_MSG ("GUI_RELEASE_AND_UPDATE")
#define GUI_DRAG_AND_DROP(source,object) _GUI_MSG ("GUI_DRAG_AND_DROP")
#define GUI_DRAG_AND_DROP_OBJECT() (_GUI_MSG ("GUI_DRAG_AND_DROP_OBJECT"), nil)
#define GUI_EVENT_SYNC() _GUI_MSG ("GUI_EVENT_SYNC")
#define GUI_EVENT_ASYNC() (_GUI_MSG ("GUI_EVENT_ASYNC"), 0)

// for MessageProbeWidget
#define GUI_MAKE_FRAME(widget) _GUI_MSG ("GUI_MAKE_FRAME")
#define GUI_PACK(widget) _GUI_MSG ("GUI_PACK")
// for VarProbeWidget
#define GUI_FOCUS(widget) _GUI_MSG ("GUI_FOCUS")

#import <awtobjc/global.h>

#define GUI_INIT(arguments)  initAWTObjc (arguments)
#endif

#if !defined(GNUSTEP) && !defined(SWARM_OSX)
#ifndef USE_JAVA
#import <tkobjc/common.h>
extern void initTkObjc (id arguments);

#define GUI_BEEP() tkobjc_ringBell ()
#define GUI_UPDATE() tkobjc_update ()
#define GUI_UPDATE_IDLE_TASKS() tkobjc_updateIdleTasks (NO)
#define GUI_UPDATE_IDLE_TASKS_AND_HOLD() tkobjc_updateIdleTasks (YES)
#define GUI_RELEASE_AND_UPDATE() tkobjc_releaseAndUpdate () 
#define GUI_DRAG_AND_DROP(source,object) tkobjc_dragAndDrop (source, object)
#define GUI_DRAG_AND_DROP_OBJECT() tkobjc_drag_and_drop_object ()
#define GUI_EVENT_SYNC() tkobjc_doOneEventSync ()
#define GUI_EVENT_ASYNC() tkobjc_doOneEventAsync ()

// for MessageProbeWidget
#define GUI_MAKE_FRAME(widget) tkobjc_makeFrame (widget)
#define GUI_PACK(widget) tkobjc_pack (widget)
// for VarProbeWidget
#define GUI_FOCUS(widget) tkobjc_focus (widget)

#define GUI_INIT(arguments) initTkObjc (arguments)
#endif
#endif // !GNUSTEP

@class Button;
@class ButtonPanel;
@class CheckButton;
@class ClassDisplayHideButton;
@class ClassDisplayLabel;
@class ClassDisplayWidget;
@class CompleteProbeDisplayLabel;
@class Form;
@class Frame;
@class Label;
@class MessageProbeEntry;
@class ProbeCanvas;
@class SimpleProbeDisplayHideButton;
@class SuperButton;
@class VarProbeEntry;
@class VarProbeLabel;
@class Widget;

@class Colormap;
@class Entry;
@class Graph;
@class Canvas;
@class CanvasItem;
@class Histogram;
@class LinkItem;
@class OvalNodeItem;
@class Pixmap;
@class RectangleNodeItem;
@class Raster;
@class ZoomRaster;

@class Circle;
#if SWARM_OSX
@class SwarmLine;
#else
@class Line;
#endif
@class NodeItem;
@class Rectangle;
@class ScheduleItem;
@class TextItem;

#define ButtonLeft 1
#define ButtonMiddle 2
#define ButtonRight 3
