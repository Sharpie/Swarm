#import <objectbase.h>

//S: GUI interface for Swarm.

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
//S: Widget base class.

//D: All graphical widgets inherit from the Widget base class. Widget
//D: defines most of the behaviour needed: Widgets are created by the user
//D: with a particular parent, and then "pack"ed in order to draw them on
//D: the screen.  All widgets have three essential things: a widget name
//D: used when running Tcl code, an Objective C name when sending messages
//D: from Tcl to those objects, and a parent.

//M: When a widget is created it needs to be given a parent.  The parent
//M: widget will be the widget's containing window. If no parent is given
//M: (ie, a parent of nil), then a toplevel Frame will be allocated
//M: automatically
+ createParent: parent;

//M: Roughly, packing a widget makes it draw on the screen. The Tk packer
//M: allows complicated options to control widget layout. See documentation
//M: on Tk to learn more about packing details.
- pack;

- packFill;
- packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- packFillLeft: (BOOL)expandFlag;
- packToRight : widget;
- packForgetAndExpand;

//M: Set the containing window of the widget.
- setParent: parent;

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
- setPositionX: (int)x Y: (int)y;

//M: Set the title on the widget.
- setWindowTitle: (const char *)title;

//M: Compute the widget name for a component widget.
- (const char *)makeWidgetNameFor: widget;

//M: Get the widget name.
- (const char *)getWidgetName;

//M: Set the widget name using the parent as context.
- setWidgetNameFromParent: parent;

//M: Set the widget name using a hypothetical parent name.
- setWidgetNameFromParentName: (const char *)parentWidgetName;

//M: Get the height of the widget.
- (unsigned)getHeight;

//M: Get the widget the widget.
- (unsigned)getWidth;
@end

@protocol Widget <_Widget, SwarmObject>
@end

@protocol _WindowGeometryRecord
//S: A container for window geometry information.

//D: A container for window geometry information that implements
//D: archiving methods.

//M: Get a string describing the window geometry.
- (const char *)getWindowGeometry;

//M: Set the window geometry from geometry string.
- setWindowGeometry :(const char *)theWindowGeometryString;

//M: Print the geometry to a stream.
- (void)describe: outputCharStream;

//M: Load window geometry object using expression object.
- in: expr;

//M: Create window geometry object using expression object.
+ in: aZone expr: expr;

//M: Print window geometry to stream.
- out: outputCharStream; 
@end

@protocol WindowGeometryRecord <_WindowGeometryRecord, SwarmObject>
@end

@protocol _ArchivedGeometryWidget
//S: Base class for widgets that archive geometry.

//D: Subclasses of this class inherit the ability to archive
//D: their window geometry.  This class also provides an interface
//D: to destroy notification.

//M: Call a method if we are destroyed.
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)destroyNotificationMethod;

//M: Prevent calling the destroy notification method.
- disableDestroyNotification;

//M: Called to set a name for archiving.
- setWindowGeometryRecordName: (const char *)recordName;
@end

@protocol ArchivedGeometryWidget <_ArchivedGeometryWidget, Widget>
@end

@protocol _Frame
//S: Encapsulation of toplevels.

//D: Frames are boxes other widgets fit in. They correspond to the Tk
//D: "frame" and "toplevel" widgets. Frames can be new windows, or
//D: subwindows in an existing window. You only need to create frames
//D: yourself if building complicated composite widgets: by default, a
//D: frame will be built automatically for widgets without parents.

//M: Determines whether or not a frame has a border.
- setReliefFlag: (BOOL)reliefFlag;

//M: Determines the width of the border, if any.
- setBorderWidth: (int)width;

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
@end

@protocol _Canvas
//S: An interface to Tk canvas semantics.

//D: The Canvas widget allows display of a diverse range of graphical objects.

//M: Create the canvas.
- createEnd;
@end

@protocol Canvas <_Canvas, ArchivedGeometryWidget>
@end

@protocol _ProbeCanvas
//S: A canvas type for probe displays.

//D: ProbeCanvas is a Canvas that implements the general appearance and
//D: interface of a probe display.

//M: Indicates the presence or absence of a horizontal scroll bar.
- setHorizontalScrollbarFlag: (BOOL)horizontalScrolbarFlag;
@end

@protocol ProbeCanvas <_ProbeCanvas, Canvas>
@end

@protocol _GraphElement
//S: Contains a set of related data for display.

//D: A GraphElement accumulates a related set of data for display,
//D: including attributes for the set.

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
@end

@protocol GraphElement <_GraphElement, SwarmObject>
@end

@protocol _Graph
//S: A time series graph tool.

//D: A time series graph tool, based on BLT's graph widget.  Graph
//D: currently implements just a basic graph with multiple datasets, but
//D: should eventually support scaling and scrolling. For each Graph you
//D: create one or many GraphElements, one per dataset to
//D: plot. GraphElements can be configured for appearance, and data can be
//D: added to the element to draw.

//M: Set the title for the graph.
- setTitle: (const char *)title;
//M: Set the axis labels for the graph.
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
//M: Builds a new GraphElement to plot data with.
- (id <GraphElement>)createElement;
//M: Whether to autoscale every timestep or instead to jump scale.
- setScaleModeX: (int)xs Y: (int)ys;
//M: Sets the Y ranges for the graph.  Turns off autoscaling.
- setRangesYMin: (double)miny Max:(double)maxy;
//M: Sets the ranges for the graph.  Turns off autoscaling.
- setRangesXMin: (double)minx Max:(double)maxx YMin: (double)miny Max: (double)maxy;
@end

@protocol Graph <_Graph, ArchivedGeometryWidget>
@end

@protocol _Histogram
//S: Histogram display tool.

//D: In Tk, this is based on BLT's barchart. 
//D: The number of bins is fixed at creation time, then the user hands
//D: the Histogram an array of datapoints (double or int) to display
//D: (or optionally an array of datapoints and locations where the bars
//D: should be drawn (specified as doubles).

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
@end

@protocol _Label
//S: A widget with text.

//D: A widget with text.

//M: Set the text to write in the label.
- setText: (const char *)text;
@end

@protocol Label <_Label, Widget>
@end

@protocol _ClassDisplayLabel
//S: A label for displaying class names.

//D: This widget is used internally by ClassDisplayWidget.

//M: Create a blue, left-justified label.
- createEnd;
@end

@protocol ClassDisplayLabel <_ClassDisplayLabel, Label>
@end

@protocol _VarProbeLabel
//S: A label for displaying variable names.

//D: This widget is used internally by VarProbeWidget.

//M: Create a right-justified label.
- createEnd;
@end

@protocol VarProbeLabel <_VarProbeLabel, Label>
@end

@protocol _CompleteProbeDisplayLabel
//S: A class label used in a SimpleProbeDisplay.

//D: This widget is used internally by SimpleProbeDisplay.
//D: It is used to set up the mouse bindings to get a CompleteProbeDisplay,
//D: and to set up drag and drop.

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
@end

@protocol _Button
//S: A button widget.

//D: A button widget that, when pressed, sends a method to a target object.
- setText: (const char *)text;
//D: Set the target and selector for button.
- setButtonTarget: target method: (SEL)method;
@end

@protocol Button <_Button, Widget>
@end

@protocol _ClassDisplayHideButton
//S: The hide button used by a CompleteProbeDisplay.

//D: A button that handles the dismissal of class widgets on a
//D: ClassDisplayWidget (for CompleteProbeDisplay).

- setSubWidget: subWidget;
- setUser: user;
- setOwner: owner;
@end

@protocol ClassDisplayHideButton <_ClassDisplayHideButton, Button>
@end

@protocol _SimpleProbeDisplayHideButton
//S: The hide button used by a SimpleProbeDisplay.

//D: A button that handles the dismissal of a SimpleProbeDisplay.

//M: The probe display in use.
- setProbeDisplay: probeDisplay;
@end

@protocol SimpleProbeDisplayHideButton <_SimpleProbeDisplayHideButton, Button>
@end

@protocol _SuperButton
//S: Request superclass in ClassDisplayWidget.

//D: A button used by ClassDisplayWidget to ask for superclass.

- createEnd;
- setSuperWidget: superWidget;
- setOwner: owner;
- setUser: user;
@end

@protocol SuperButton <_SuperButton, Button>
@end

@protocol _InputWidget
//S: Abstract superclass for widgets that take input.

//D: InputWidgets get their input in one of two ways: by being readable, or
//D: by being linked to a C variable.

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

@protocol _Entry
//S: Handles text-field input.

//D: Handles text-field input.

- createEnd;

//M: Set the value of the widget, replacing the visible text in the widget.
- setValue: (const char *)value;

//M: This method aborts
- setHeight: (unsigned)h; // since this isn't possible with Tk, it will abort.
@end

@protocol Entry <_Entry, _InputWidget, Widget>
@end

@protocol _MessageProbeEntry
//S: A widget for arguments to a message probe.

//D: An Entry widget for MessageProbe arguments.

//M: Indicates whether the type of this entry is an id.
- setIdFlag: (BOOL)idFlag;

//M: Indicates the argument number.
- setArg: (int)arg;
+ createBegin: aZone;
- createEnd;
@end

@protocol MessageProbeEntry <_MessageProbeEntry, Entry>
@end

@protocol _VarProbeEntry
//S: A widget for variable probes.

//D: An Entry widget for VarProbes.

//M: Indicates whether the entry is editable or not.
- setInteractiveFlag: (BOOL)interactiveFlag;

//M: Indicate the widget that is using this widget.
- setOwner: owner;

//M: Set the type of object edited by this entry.
- setProbeType: (char)probeType;

- createEnd;
@end

@protocol VarProbeEntry <_VarProbeEntry, Entry>
@end

@protocol _ButtonPanel
//S: Several buttons bound together in one frame.

//D: Several buttons bound together in one frame.

//M: Set a default target for use with addButtonName:method:.
- setButtonTarget: target;

//M: Create a new button, and set both a target and method.
- addButtonName: (const char *)name target: target method: (SEL)sel;

//M: Create a new button, and set the method, using the default target.
- addButtonName: (const char *)name method: (SEL)sel;
@end

@protocol ButtonPanel <_ButtonPanel, Frame>
@end

@protocol _Form
//S: A set of Entry widgets bound together in one frame.

//D: A set of Entry widgets bound together in one frame.

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
@end

@protocol _CheckButton
//S: A check box on/off selection widget.

//D: A check box on/off selection widget.

//M: Get on/off status.
- (BOOL)getBoolValue;

//M: Turn the widget value and check button on or off.
- setBoolValue: (BOOL)v;
@end

@protocol CheckButton <_CheckButton, Widget>
@end

typedef unsigned char Color; 
typedef unsigned long PixelValue;

@protocol _Colormap
//S: An class for creating a color palette for use with a Raster.

//D: Mechanism used to map numbers in the range [0, 255] to colour
//D: names. Create an XColormap, allocate colours in it, and pass it to a
//D: Raster widget for drawing.

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
@end

@class Raster;

@protocol Drawer
- drawX: (int)x Y: (int)y;
@end

@protocol _Raster
//S: A two dimension color display class.

//D: 2 dimensional, colour pixel images. Raster is based on a Tk frame widget
//D: with our own code for fast display of images. You can draw coloured dots
//D: on a Raster, or generic Drawers. Raster widgets are
//D: double buffered - the pixels you draw are not actually put on the screen
//D: until drawSelf is called. In addition, Rasters handle mouse clicks.

//M: Draw a point at the given coordinates with the given color.
- drawPointX: (int)x Y: (int)y Color: (Color)c;

//M: Set the palette for this raster.
- setColormap: (id <Colormap>)c;

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
@end

@protocol _ZoomRaster
//S: A zoomable Raster.

//D: ZoomRaster is a subclass of Raster that implements a zoomable image. It
//D: handles translation between logical coordinates and screen coordinates.

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
@end

@protocol _Pixmap
//S: A class for drawing color bitmaps on a Raster.

//D: A class for drawing color bitmaps on a Raster.  The bitmaps are
//D: stored in the Portable Network Graphics format.

//M: Set the bitmap file to load.
- setFile: (const char *)filename;

//M: Set the raster that the pixmap will be shown on.
//M: This is a required call.  It's used to needed to determine the
//M: color palette in use.
- setRaster: (id <Raster>)raster;

//M: Get the width of the bitmap in pixels.
- getWidth;

//M: Get the height of the bitmap in pixels.
- getHeight;

//M: Load the bitmap file and create the backend pixmap object.
- createEnd;

//M: Draw the pixmap on the current raster at the given position.
- drawX: (int)x Y: (int)y;
@end

@protocol Pixmap <_Pixmap, Drawer, Create>
@end

@protocol _CanvasAbstractItem
- setCanvas: canvas;
- setTargetId: target;
- setClickSel: (SEL)sel;
- setMoveSel: (SEL)sel;
- setPostMoveSel: (SEL)sel;
- createItem;
- createBindings;
- createEnd;
- clicked;
- initiateMoveX: (long)delta_x Y: (long)delta_y; 
@end

@protocol CanvasAbstractItem <_CanvasAbstractItem, SwarmObject>
@end

@protocol _CanvasItem
- createItem;
- createBindings;
- initiateMoveX: (long)delta_x Y: (long)delta_y; 
@end

@protocol CanvasItem <_CanvasItem, CanvasAbstractItem>
@end

@protocol _CompositeItem
//S: A CanvasItem with several pieces.

//D: A CompositeItem is a CanvasItem that consists of several pieces.
//D: CompositeItem is an abstract superclass.

//M: Must be implemented by subclass.
- moveX: (long)delta_x Y: (long)delta_y;

//M: Prepares for movement of the item within the canvas.
- initiateMoveX: (long)delta_x Y: (long)delta_y;
@end

@protocol CompositeItem <_CompositeItem, CanvasAbstractItem>
@end

@protocol _NodeItem
- setX: (int)x Y: (int)y;
- (int)getX;
- (int)getY;
- setString: (const char *)string;
- setFont: (const char *)the_font;
- setColor: (const char *)aColor;
- setBorderColor: (const char *)aColor;
- setBorderWidth: (int)aVal;
- createBindings;
@end

@protocol _LinkItem
- setFrom: from;
- setTo: to;
@end

@protocol LinkItem <_LinkItem, CompositeItem>
@end

@protocol NodeItem <_NodeItem, CompositeItem>
@end

@protocol _OvalNodeItem
@end

@protocol OvalNodeItem <_OvalNodeItem, NodeItem>
@end

@protocol _RectangleNodeItem
@end

@protocol RectangleNodeItem <_RectangleNodeItem, NodeItem>
@end

@protocol _TextItem
- setX: (int)x Y: (int)y;
- setText: (const char *)the_text;
- setFont: (const char *)the_font;
- createItem; 
@end

@protocol TextItem <_TextItem, CanvasItem>
@end

@protocol _Circle
- setX: (int)x Y: (int)y;
- setRadius: (int)r;
- createItem;
- reportClick;
- (int)reportMoveX: (int)d_x Y: (int)d_y;
@end

@protocol Circle <_Circle, CanvasItem>
@end

@protocol _Rectangle
- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;
- createItem;
@end

@protocol Rectangle <_Rectangle, CanvasItem>
@end

@protocol _Line
- setTX: (int)tx TY: (int)ty LX: (int)lx LY: (int)ly;
- createItem;
@end

@protocol Line <_Line, CanvasItem>
@end

#ifndef USE_JAVA
#import <tkobjc/common.h>
void initTkObjc (id arguments);

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
#else
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

