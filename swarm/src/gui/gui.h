#import <objectbase.h>

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

@protocol _Widget
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
- setWidgetNameFromParent: parent;

//M: Set the widget name using a hypothetical parent name.
- setWidgetNameFromParentName: (const char *)parentWidgetName;

- createEnd;

USING
//M: Roughly, packing a widget makes it draw on the screen. The Tk packer
//M: allows complicated options to control widget layout. See documentation
//M: on Tk to learn more about packing details.
- pack;

- packFill;
- packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- packFillLeft: (BOOL)expandFlag;
- packToRight : widget;
- packForgetAndExpand;

//M: Get the containing window of the widget.
- getParent;

//M: Enable or disable the widget.
- setActiveFlag: (BOOL)activeFlag;

//M: Set the width of the widget.
- setWidth: (unsigned)width;

//M: Set the height of the widget.
- setHeight: (unsigned)height;

//M: Set the width and height of the widget.
- setWidth: (unsigned)width Height: (unsigned)height;

//M: Set the position of the widget.
- setX: (int)x Y: (int)y;

//M: Set the title on the widget.
- setWindowTitle: (const char *)title;

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
@end

@protocol Widget <_Widget, SwarmObject>
//S: Widget base class.

//D: All graphical widgets inherit from the Widget base class. Widget
//D: defines most of the behaviour needed: Widgets are created by the user
//D: with a particular parent, and then "pack"ed in order to draw them on
//D: the screen.  All widgets have three essential things: a widget name
//D: used when running Tcl code, an Objective C name when sending messages
//D: from Tcl to those objects, and a parent.
@end

@protocol _WindowGeometryRecord
CREATING
//M: Create window geometry object using expression object.
+ in: aZone expr: expr;

USING
//M: Load window geometry object using expression object.
- in: expr;

//M: Print window geometry to stream.
- out: outputCharStream; 

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

@protocol WindowGeometryRecord <_WindowGeometryRecord, SwarmObject>
//S: A container for window geometry information.

//D: A container for window geometry information that implements
//D: archiving methods.
@end

@protocol _ArchivedGeometryWidget
CREATING
+ createBegin: aZone;

//M: Called to set a name for archiving.
- setWindowGeometryRecordName: (const char *)recordName;

- loadWindowGeometryRecord;
- registerAndLoad;
USING
//M: Call a method if we are destroyed.
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)destroyNotificationMethod;

//M: Prevent calling the destroy notification method.
- disableDestroyNotification;

- updateArchiver;
- getDestroyedFlag;
- (void)drop;
@end

@protocol ArchivedGeometryWidget <_ArchivedGeometryWidget, Widget>
//S: Base class for widgets that archive geometry.

//D: Subclasses of this class inherit the ability to archive
//D: their window geometry.  This class also provides an interface
//D: to destroy notification.
@end

@protocol _Frame
CREATING
//M: Determines the width of the border, if any.
- setBorderWidth: (int)width;

//M: Determines whether or not a frame has a border.
- setReliefFlag: (BOOL)reliefFlag;

- createEnd;

USING
//M: Take the frame off screen.
- withdraw;

//M: Deiconify the frame.
- deiconify;

//M: Make sure the that the geometry is `reasonable'.
- assertGeometry;

//M: Move to the northwest corner of the screen.
- assertPosition;
@end

@protocol Frame <_Frame, ArchivedGeometryWidget>
//S: Encapsulation of toplevels.

//D: Frames are boxes other widgets fit in. They correspond to the Tk
//D: "frame" and "toplevel" widgets. Frames can be new windows, or
//D: subwindows in an existing window. You only need to create frames
//D: yourself if building complicated composite widgets: by default, a
//D: frame will be built automatically for widgets without parents.
@end

@protocol _Canvas
CREATING
//M: Create the canvas.
- createEnd;
@end

@protocol Canvas <_Canvas, ArchivedGeometryWidget>
//S: An interface to Tk canvas semantics.

//D: The Canvas widget allows display of a diverse range of graphical objects.
@end

@protocol _ProbeCanvas
CREATING
//M: Indicates the presence or absence of a horizontal scroll bar.
- setHorizontalScrollbarFlag: (BOOL)horizontalScrolbarFlag;

- createEnd;
@end

@protocol ProbeCanvas <_ProbeCanvas, Canvas>
//S: A canvas type for probe displays.

//D: ProbeCanvas is a Canvas that implements the general appearance and
//D: interface of a probe display.
@end

@protocol _GraphElement
CREATING
- setOwnerGraph: ownerGraph;
- createEnd;
+ createOwnerGraph: ownerGraph;

USING
//M: Set the label for the element.
- setLabel: (const char *)label;

//M: Set the color for the element.
- setColor: (const char *)colorName;

//M: Set the dash style, 0 means solid.
- setDashes: (int)dashesVal;

//M: Set the symbol for the element.
- setSymbol: (const char *)symbol;

//M: Add a new data point.
- addX: (double)x Y: (double)y;

//M: Clear the data to be displayed.
- resetData;

//M: Set the line width of the element.
- setWidth: (unsigned)w;
@end

@protocol GraphElement <_GraphElement, SwarmObject>
//S: Contains a set of related data for display.

//D: A GraphElement accumulates a related set of data for display,
//D: including attributes for the set.
@end

@protocol _Graph
CREATING
- createEnd;

USING
//M: Set the title for the graph.
- setTitle: (const char *)title;
//M: Set the axis labels for the graph.
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
//M: Builds a new GraphElement to plot data with.
- (id <GraphElement>)createElement;
//M: Whether to autoscale every timestep or instead to jump scale.
- setScaleModeX: (BOOL)xs Y: (BOOL)ys;
//M: Sets the X ranges for the graph.  Turns off autoscaling.
- setRangesXMin: (double)minx Max:(double)maxx;
//M: Sets the Y ranges for the graph.  Turns off autoscaling.
- setRangesYMin: (double)miny Max:(double)maxy;
//M: Sets the ranges for the graph.  Turns off autoscaling.
- setRangesXMin: (double)minx Max:(double)maxx YMin: (double)miny Max: (double)maxy;
@end

@protocol Graph <_Graph, ArchivedGeometryWidget>
//S: A time series graph tool.

//D: A time series graph tool, based on BLT's graph widget.  Graph
//D: currently implements just a basic graph with multiple datasets, but
//D: should eventually support scaling and scrolling. For each Graph you
//D: create one or many GraphElements, one per dataset to
//D: plot. GraphElements can be configured for appearance, and data can be
//D: added to the element to draw.
@end


@protocol _Histogram
CREATING
- createEnd;

USING
//M: Initialize the histogram, tell it how big its dataset is. Labels and
//M: Colors are arrays of strings (one string per point) for text labels
//M: and the colours of the bars (the last two arguments are optional).
- setNumPoints: (int)n
        Labels: (const char * const *)l
        Colors: (const char * const *)c;

//M: Set the title of the histogram.
- setTitle: (const char *)title;

//M: Set the width of the bars.
- setBarWidth: (double)step;

//M: Set the X range and step size for the histogram.
- setXaxisMin: (double)min max: (double)max step: (double)step;

//M: Set the axis labels.
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

//M: Set the text that describes a specified number of outliers.
- setActiveOutlierText: (int)outliers count: (int)count;

//M: Hide the legend on the histogram.
- hideLegend;

//M: Draw the (integer) data in the histogram.
- drawHistogramWithInt: (int *)points;

//M: Draw the (double) data in the histogram.
- drawHistogramWithDouble: (double *)points;

//M: Draw the (integer) data in the histogram at particular offsets.
- drawHistogramWithInt: (int *)points atLocations: (double *)locations;

//M: Draw the (double) data in the histogram at particular offsets.
- drawHistogramWithDouble: (double *)points atLocations: (double *)locations;

- setupZoomStack;
- setupActiveOutlierMarker;
- setupActiveItemInfo;
@end

@protocol Histogram <_Histogram, ArchivedGeometryWidget>
//S: Histogram display tool.

//D: In Tk, this is based on BLT's barchart. 
//D: The number of bins is fixed at creation time, then the user hands
//D: the Histogram an array of datapoints (double or int) to display
//D: (or optionally an array of datapoints and locations where the bars
//D: should be drawn (specified as doubles).
@end

@protocol _Label
CREATING
- createEnd;

USING
//M: Set the text to write in the label.
- setText: (const char *)text;
@end

@protocol Label <_Label, Widget>
//S: A widget with text.

//D: A widget with text.
@end

@protocol _ClassDisplayLabel
CREATING
//M: Create a blue, left-justified label.
- createEnd;
@end

@protocol ClassDisplayLabel <_ClassDisplayLabel, Label>
//S: A label for displaying class names.

//D: This widget is used internally by ClassDisplayWidget.
@end

@protocol _VarProbeLabel
CREATING
//M: Create a right-justified label.
- createEnd;
@end

@protocol VarProbeLabel <_VarProbeLabel, Label>
//S: A label for displaying variable names.

//D: This widget is used internally by VarProbeWidget.
@end

@protocol _CompleteProbeDisplayLabel
CREATING
//M: Sets probe display which the widget represents.
- setProbeDisplay: probeDisplay;

//M: Sets the object that the probe display represents.
- setProbedObject: probedObject;

//M: Sets the ProbeDisplayMananger that is in use.
- setProbeDisplayManager: probeDisplayManager;

//M: Performs drag and drop configruation, Button3 to get a
//M: CompleteProbeDisplay, and highlighting.
- createEnd;
@end

@protocol CompleteProbeDisplayLabel <_CompleteProbeDisplayLabel, Label>
//S: A class label used in a SimpleProbeDisplay.

//D: This widget is used internally by SimpleProbeDisplay.
//D: It is used to set up the mouse bindings to get a CompleteProbeDisplay,
//D: and to set up drag and drop.
@end

@protocol _Button
CREATING
- createEnd;

USING
//M: Set the text for button.
- setText: (const char *)text;

//M: Set the target and selector for button.
- setButtonTarget: target method: (SEL)method;
@end

@protocol Button <_Button, Widget>
//S: A button widget.

//D: A button widget that, when pressed, sends a method to a target object.
@end

@protocol _ClassDisplayHideButton
CREATING
- setSubWidget: subWidget;
- setUser: user;
- setOwner: owner;
@end

@protocol ClassDisplayHideButton <_ClassDisplayHideButton, Button>
//S: The hide button used by a CompleteProbeDisplay.

//D: A button that handles the dismissal of class widgets on a
//D: ClassDisplayWidget (for CompleteProbeDisplay).
@end

@protocol _SimpleProbeDisplayHideButton
CREATING
//M: The probe display in use.
- setProbeDisplay: probeDisplay;

- createEnd;
@end

@protocol SimpleProbeDisplayHideButton <_SimpleProbeDisplayHideButton, Button>
//S: The hide button used by a SimpleProbeDisplay.

//D: A button that handles the dismissal of a SimpleProbeDisplay.
@end

@protocol _SuperButton
CREATING
- createEnd;
- setSuperWidget: superWidget;
- setOwner: owner;
- setUser: user;
@end

@protocol SuperButton <_SuperButton, Button>
//S: Request superclass in ClassDisplayWidget.

//D: A button used by ClassDisplayWidget to ask for superclass.
@end

@protocol _InputWidget
CREATING
- createEnd;

USING
//M: Get the string value of the widget.
- (const char *)getValue;

//M: Attach the widget value to an integer.
- linkVariableInt: (int *)p;

//M: Attach the widget value to a double.
- linkVariableDouble: (double *)p;

//M: Attach the widget value to a boolean.
- linkVariableBoolean: (BOOL *)p;

//M: Set the string value of the widget. 
//M: This must be implemented by a subclass.
- setValue: (const char *)v;
@end

@protocol InputWidget <_InputWidget, Widget>
//S: Abstract superclass for widgets that take input.

//D: InputWidgets get their input in one of two ways: by being readable, or
//D: by being linked to a C variable.
@end

@protocol _Entry
CREATING
- createEnd;

USING
//M: Set the value of the widget, replacing the visible text in the widget.
- setValue: (const char *)value;

//M: This method aborts
- setHeight: (unsigned)h; // since this isn't possible with Tk, it will abort.
@end

@protocol Entry <_Entry, InputWidget>
//S: Handles text-field input.

//D: Handles text-field input.
@end

@protocol _MessageProbeEntry
CREATING
//M: Indicates whether the type of this entry is an id.
- setIdFlag: (BOOL)idFlag;

//M: Indicates the argument number.
- setArg: (int)arg;
+ createBegin: aZone;
- createEnd;
@end

@protocol MessageProbeEntry <_MessageProbeEntry, Entry>
//S: A widget for arguments to a message probe.

//D: An Entry widget for MessageProbe arguments.
@end

@protocol _VarProbeEntry
CREATING
//M: Indicates whether the entry is editable or not.
- setInteractiveFlag: (BOOL)interactiveFlag;

//M: Indicate the object that is using this widget.
- setOwner: owner;

//M: Set the variable probe associated with this widget.
- setVarProbe: varProbe;

- createEnd;
USING
- getVarProbe;
@end

@protocol VarProbeEntry <_VarProbeEntry, Entry>
//S: A widget for variable probes.

//D: An Entry widget for VarProbes.
@end

@protocol _ButtonPanel
USING
//M: Set a default target for use with addButtonName:method:.
- setButtonTarget: target;

//M: Create a new button, and set both a target and method.
- addButtonName: (const char *)name target: target method: (SEL)sel;

//M: Create a new button, and set the method, using the default target.
- addButtonName: (const char *)name method: (SEL)sel;
@end

@protocol ButtonPanel <_ButtonPanel, Frame>
//S: Several buttons bound together in one frame.

//D: Several buttons bound together in one frame.
@end

@protocol _Form
CREATING
- createEnd;

USING
//M: The width of all the Entry widgets.
- setEntryWidth: (int)ew;

//M: Add a boolean CheckButton widget.
- addLineName: (const char *)n Boolean: (BOOL *)p;

//M: Add an Entry to get an integer.
- addLineName: (const char *)n Int: (int *)p;

//M: Add an Entry to get a double.
- addLineName: (const char *)n Double: (double *)p;
@end

@protocol Form <_Form, Widget>
//S: A set of Entry widgets bound together in one frame.

//D: A set of Entry widgets bound together in one frame.
@end

@protocol _CheckButton
CREATING
- createEnd;

USING
//M: Get on/off status.
- (BOOL)getBoolValue;

//M: Turn the widget value and check button on or off.
- setBoolValue: (BOOL)v;
@end

@protocol CheckButton <_CheckButton, InputWidget>
//S: A check box on/off selection widget.

//D: A check box on/off selection widget.
@end

typedef unsigned char Color; 
typedef unsigned long PixelValue;

@protocol _Colormap
CREATING
- createEnd;

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
@end

@protocol Colormap <Create, _Colormap>
//S: An class for creating a color palette for use with a Raster.

//D: Mechanism used to map numbers in the range [0, 255] to colour
//D: names. Create an XColormap, allocate colours in it, and pass it to a
//D: Raster widget for drawing.
@end

@class Raster;

@protocol Drawer
//S: The interface used by Raster to draw an arbitrary object.

//D: The interface used by Raster to draw an arbitrary object.
//D: Pixmap uses this.
USING
- drawX: (int)x Y: (int)y;
@end

@protocol _Raster
CREATING
+ createBegin: aZone;
- createEnd;

USING
//M: Set the palette for this raster.
- setColormap: (id <Colormap>)c;

//M: Draw a point at the given coordinates with the given color.
- drawPointX: (int)x Y: (int)y Color: (Color)c;

//M: Draw the raster to the display.
- drawSelf;

//M: Set the size of the raster.
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight;

//M: Configure at mouse button to send a message to a given client object.
- setButton: (int)n Client: c Message: (SEL)sel;

//M: Fill a rectangle of given geometry and color.
- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)color;

//M: Draw an ellipse of given geometry, pen width, and color.
- ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
      Width: (unsigned)penWidth Color: (Color)c;

//M: Draw a line of given geometry, pen width, and color.
- lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
   Width: (unsigned)penWidth Color: (Color)c;

//M: Draw a rectangle of given geometry, pen width, and color.
- rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
        Width: (unsigned)penWidth Color: (Color)c;

//M: Draw an object at a given position.
- draw: (id <Drawer>)drawer X: (int)x Y: (int)y;

//M: Erase the raster.
- erase;
@end

@protocol Raster <ArchivedGeometryWidget, _Raster>
//S: A two dimension color display class.

//D: 2 dimensional, colour pixel images. Raster is based on a Tk frame widget
//D: with our own code for fast display of images. You can draw coloured dots
//D: on a Raster, or generic Drawers. Raster widgets are
//D: double buffered - the pixels you draw are not actually put on the screen
//D: until drawSelf is called. In addition, Rasters handle mouse clicks.
@end

@protocol _ZoomRaster
CREATING
- createEnd;

USING
//M: Make the raster bigger.
- increaseZoom;

//M: Make the raster smaller.
- decreaseZoom;

//M: Get the current zoom factor.
- (unsigned)getZoomFactor;

//M: Set the zoom factor.
- setZoomFactor: (unsigned)z;

//M: Reconfigures the ZoomRaster when the window is resized.
- handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight;
@end

@protocol ZoomRaster <Raster, _ZoomRaster>
//S: A zoomable Raster.

//D: ZoomRaster is a subclass of Raster that implements a zoomable image. It
//D: handles translation between logical coordinates and screen coordinates.
@end

@protocol _Pixmap
CREATING
//M: Create a pixmap from a widget, or from the root window if widget is nil.
+ create: aZone widget: widget;

//M: Create a pixmap from a PNG file.
+ create: aZone file: (const char *)filename;
SETTING
//M: Set the raster that the pixmap will be shown on.
//M: It's used to augment raster the color palette as necessary.
- setRaster: (id <Raster>)raster;

USING
//M: Get the width of the bitmap in pixels.
- (unsigned)getWidth;

//M: Get the height of the bitmap in pixels.
- (unsigned)getHeight;

//M: Draw the pixmap on the current raster at the given position.
- drawX: (int)x Y: (int)y;

//M: Save the pixmap to a file.
- save: (const char *)filename;

- (void)drop;
@end

@protocol Pixmap <_Pixmap, Drawer, Create>
//S: A class for drawing color bitmaps on a Raster.

//D: A class for drawing color bitmaps on a Raster.  The bitmaps are
//D: stored in the Portable Network Graphics format.
@end

@protocol _CanvasAbstractItem
CREATING
//M: Method to be implemented by subclass.
- createItem;

//M: Method to be implemented by subclass.
- createBindings;

//M: Calls createItem and createBindings.
- createEnd;

//M: Designates the id of the Canvas in which this item resides.
- setCanvas: canvas;

USING
//M: Designates the object to which this item refers.
- setTargetId: target;

//M: Sets the message that will be sent upon a click on this item.
- setClickSel: (SEL)sel;

//M: Sets the message that will effect the motion of the item on the canvas.
- setMoveSel: (SEL)sel;

//M: Sets the message that will dictate what happens after the item is moved.
- setPostMoveSel: (SEL)sel;

//M: Called when a mouse click occurs.
- clicked;

//M: Method to be implemented by subclass.
- initiateMoveX: (long)delta_x Y: (long)delta_y; 
@end

@protocol CanvasAbstractItem <_CanvasAbstractItem, SwarmObject>
//S: An abstract class for items on a Canvas.

//D: CanvasAbstractItem is the root class of all items drawn on a Canvas.
@end

@protocol _CanvasItem
CREATING
//M: Establishes the bindings for the buttons.
- createBindings;

USING
//M: Prepares for movement of the item within the canvas.
- initiateMoveX: (long)delta_x Y: (long)delta_y; 
@end

@protocol CanvasItem <_CanvasItem, CanvasAbstractItem>
//S: An abstract superclass for simple Canvas items.

//D: An abstract superclass for non-composite Canvas items.
@end

@protocol _CompositeItem
USING
//M: Must be implemented by subclass.
- moveX: (long)delta_x Y: (long)delta_y;

//M: Prepares for movement of the item within the canvas.
- initiateMoveX: (long)delta_x Y: (long)delta_y;
@end

@protocol CompositeItem <_CompositeItem, CanvasAbstractItem>
//S: A CanvasItem with several pieces.

//D: A CompositeItem is a CanvasItem that consists of several pieces.
//D: CompositeItem is an abstract superclass.
@end

@protocol _NodeItem
CREATING
//M: Set the mouse bindings for a NodeItem (e.g. dragging).
- createBindings;

USING
//M: Set the position of the node.
- setX: (int)x Y: (int)y;

//M: Get the x position of the node on the canvas.
- (int)getX;

//M: Get the y position of the node on the canvas.
- (int)getY;

//M: Set the label to put on the node.
- setString: (const char *)string;

//M: Set the font with which to draw the label.
- setFont: (const char *)the_font;

//M: Set the color of the node.
- setColor: (const char *)aColor;

//M: Set the border color of the node.
- setBorderColor: (const char *)aColor;

//M: Set the width of the border.
- setBorderWidth: (int)aVal;

//M: Create the text for the node.
- createText;

//M: Create the space for the text for the node.
- createPaddedText;

@end

@protocol NodeItem <_NodeItem, CompositeItem>
//S: A class for displaying a node on a Canvas.

//D: A class for displaying a node on a Canvas.
//D: A NodeItem has a position, a font, color, border color and width.
@end

@protocol _LinkItem
CREATING
//M: Designate the node that will be the source of the link.
- setFrom: from;

//M: Designate the node that will be the destination of the link.
- setTo: to;

//M: Create a the lines that make up the link item.
- createItem;

//M: A LinkItem is passive; disable the mouse bindings.
- createBindings;

USING
//M: Set the color of the link.
- setColor: (const char *)aColor;

//M: Redraw the link (especially due to the motion of nodes).
- update;

//M: Delete the lines that make up a LinkItem.
- (void)drop;
@end

@protocol LinkItem <_LinkItem, CompositeItem>
//S: A canvas item for displaying a link between two nodes.

//D: A CompositeCanvasItem for displaying a link between two NodeItems.
@end

@protocol _OvalNodeItem
CREATING
//M: Create the OvalNodeItem.
- createItem;
@end

@protocol OvalNodeItem <_OvalNodeItem, NodeItem>
//S: A circular NodeItem.

//D: A NodeItem with a circular appearance.
@end

@protocol _RectangleNodeItem
CREATING
//M: A NodeItem with a rectangular appearance.
- createItem;
@end

@protocol RectangleNodeItem <_RectangleNodeItem, NodeItem>
//S: A rectangular NodeItem.

//D: A rectangular NodeItem.
@end

@protocol _TextItem
CREATING
//M: Set the coordinate for the center of the text.
- setX: (int)x Y: (int)y;

//M: Set the text to display.
- setText: (const char *)the_text;

//M: Set the font with which to display the text.
- setFont: (const char *)the_font;

//M: Create the TextItem.
- createItem; 
@end

@protocol TextItem <_TextItem, CanvasItem>
//S: A CanvasItem that displays text.

//D: A CanvasItem that displays text.
@end

@protocol _Circle
CREATING
//M: Set the x, y coordinates for the center of the circle.
- setX: (int)x Y: (int)y;

//M: Set the radius of the circle.
- setRadius: (int)r;

//M: Create the Circle.
- createItem;
@end

@protocol Circle <_Circle, CanvasItem>
//S: A CanvasItem that displays a circle.

//D: A CanvasItem that displays a circle.
@end

@protocol _Rectangle
CREATING
//M: Set the diagonal corner coordinates of the rectangle.
- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;

//M: Create the Rectangle.
- createItem;
@end

@protocol Rectangle <_Rectangle, CanvasItem>
//S: A CanvasItem that displays a rectangle.

//D: A CanvasItem that displays a rectangle.
@end

@protocol _Line
CREATING
//M: Set the end points of the line.
- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;

//M: Create the Line.
- createItem;
@end

@protocol Line <_Line, CanvasItem>
//S: A CanvasItem that displays a line.

//D: A CanvasItem that displays a line.
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

@class Button;
@class ButtonPanel;
@class ClassDisplayHideButton;
@class ClassDisplayLabel;
@class ClassDisplayWidget;
@class CompleteProbeDisplayLabel;
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
@class Histogram;
@class LinkItem;
@class OvalNodeItem;
@class Pixmap;
@class RectangleNodeItem;
@class Raster;
@class ZoomRaster;

#define ButtonLeft 1
#define ButtonMiddle 2
#define ButtonRight 3

