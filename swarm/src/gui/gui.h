#import <objectbase.h>

@protocol _Widget
+ createParent: parent;
- pack;
- packFill;
- packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- packFillLeft: (BOOL)expandFlag;
- packToRight : widget;
- packForgetAndExpand;
- setParent: parent;
- getParent;
- setActiveFlag: (BOOL)activeFlag;
- setWidth: (unsigned)width;
- setHeight: (unsigned)height;
- setWidth: (unsigned)width Height: (unsigned)height;
- setPositionX: (int)x Y: (int)y;
- setWindowTitle: (const char *)title;
- (const char *)makeWidgetNameFor: widget;
- (const char *)getWidgetName;
- setWidgetNameFromParent: parent;
- setWidgetNameFromParentName: (const char *)parentWidgetName;
- (unsigned)getHeight;
- (unsigned)getWidth;
@end

@protocol Widget <_Widget, SwarmObject>
@end

@protocol _WindowGeometryRecord
- (const char *)getWindowGeometry;
- setWindowGeometry : (const char *)theWindowGeometryString;
- (void)describe : outputCharStream;

- in: expr;
+ in: aZone expr: expr;
- out: outputCharStream; 
@end

@protocol WindowGeometryRecord <_WindowGeometryRecord, SwarmObject>
@end

@protocol _ArchivedGeometryWidget
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)destroyNotificationMethod;
- disableDestroyNotification;
- setWindowGeometryRecordName: (const char *)recordName;
@end

@protocol ArchivedGeometryWidget <_ArchivedGeometryWidget, Widget>
@end

@protocol _Frame
- setReliefFlag: (BOOL)reliefFlag;
- setBorderWidth: (int)width;
- withdraw;
- deiconify;
- assertGeometry;
- assertPosition;
@end

@protocol Frame <_Frame, ArchivedGeometryWidget>
@end

@protocol _Canvas
- createEnd;
@end

@protocol Canvas <_Canvas, ArchivedGeometryWidget>
@end

@protocol _ProbeCanvas
- setHorizontalScrollbarFlag: (BOOL)horizontalScrolbarFlag;
@end

@protocol ProbeCanvas <_ProbeCanvas, Canvas>
@end

@protocol _GraphElement
- setLabel: (const char *)label;
- setColor: (const char *)colorName;
- setDashes: (int)dashesVal;
- setSymbol: (const char *)symbol;
- addX: (double)x Y: (double)y;
- resetData;
@end

@protocol GraphElement <_GraphElement, SwarmObject>
@end

@protocol _Graph
- setTitle: (const char *)title;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
- (id <GraphElement>)createElement;
- setScaleModeX: (int)xs Y: (int)ys;
- setRangesYMin: (double)miny Max:(double)maxy;
- setRangesXMin: (double)minx Max:(double)maxx YMin: (double)miny Max: (double)maxy;
@end

@protocol Graph <_Graph, ArchivedGeometryWidget>
@end

@protocol _Histogram
- setNumPoints: (int)n
        Labels: (const char * const *)l
        Colors: (const char * const *)c;
- setTitle: (const char *)title;
- setBarWidth: (double)step;
- setXaxisMin: (double)min max: (double)max step: (double)step;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
- setActiveOutlierText: (int)outliers count: (int)count;

- setupZoomStack;
- hideLegend;

- setupActiveOutlierMarker;
- setupActiveItemInfo;
- drawHistogramWithInt: (int *)points;
- drawHistogramWithInt: (int *)points atLocations: (double *)locations;
- drawHistogramWithDouble: (double *)points atLocations: (double *)locations;
- drawHistogramWithDouble: (double *)points;
@end

@protocol Histogram <_Histogram, ArchivedGeometryWidget>
@end

@protocol _Label
- setText: (const char *)text;
@end

@protocol Label <_Label, Widget>
@end

@protocol _ClassDisplayLabel
- createEnd;
@end

@protocol ClassDisplayLabel <_ClassDisplayLabel, Label>
@end

@protocol _VarProbeLabel
- createEnd;
@end

@protocol VarProbeLabel <_VarProbeLabel, Label>
@end

@protocol _CompleteProbeDisplayLabel
- setProbeDisplay: probeDisplay;
- setProbedObject: probedObject;
- setProbeDisplayManager: probeDisplayManager;
- createEnd;
@end

@protocol CompleteProbeDisplayLabel <_CompleteProbeDisplayLabel, Label>
@end

@protocol _Button
- setText: (const char *)text;
- setCommand: (const char *)command;
- setButtonTarget: target method: (SEL)method;
@end

@protocol Button <_Button, Widget>
@end

@protocol _ClassDisplayHideButton
- setSubWidget: subWidget;
- setUser: user;
- setOwner: owner;
@end

@protocol ClassDisplayHideButton <_ClassDisplayHideButton, Button>
@end

@protocol _SimpleProbeDisplayHideButton
- setProbeDisplay: probeDisplay;
- setFrame: frame;
@end

@protocol SimpleProbeDisplayHideButton <_SimpleProbeDisplayHideButton, Button>
@end

@protocol _SuperButton
- createEnd;
- setSuperWidget: superWidget;
- setOwner: owner;
- setUser: user;
@end

@protocol SuperButton <_SuperButton, Button>
@end

@protocol _InputWidget
- (const char *)getValue;
- linkVariable: (void *)p Type: (int)type;
- linkVariableInt: (void *)p;
- linkVariableDouble: (void *)p;
- linkVariableBoolean: (void *)p;
- setValue: (const char *)v;
@end

@protocol _Entry
- createEnd;
- setValue: (const char *)value;
// - setWidth: (unsigned)w Height: (unsigned)h;
@end

@protocol Entry <_Entry, _InputWidget, Widget>
@end

@protocol _MessageProbeEntry
- setResultIdFlag: (BOOL)resultIdFlag;
- setArg: (int)arg;
+ createBegin: aZone;
- createEnd;
@end

@protocol MessageProbeEntry <_MessageProbeEntry, Entry>
@end

@protocol _VarProbeEntry
- setInteractiveFlag: (BOOL)interactiveFlag;
- setOwner: owner;
- setProbeType: (char)probeType;
- createEnd;
@end

@protocol VarProbeEntry <_VarProbeEntry, Entry>
@end

@protocol _ButtonPanel
- setButtonTarget: target;
- addButtonName: (const char *)n Command: (const char *)c;
- addButtonName: (const char *)n
     actionName: (const char *)action;
@end

@protocol ButtonPanel <_ButtonPanel, Frame>
@end

@protocol _Form
- setEntryWidth: (int) ew;
- addLineName: (const char *)n Variable: (void *)p Type: (int)type;
@end

@protocol Form <_Form, Widget>
@end

@protocol _CheckButton
- (BOOL)getBoolValue;
- setBoolValue: (BOOL)v;
@end

@protocol CheckButton <_CheckButton, Widget>
@end

typedef unsigned char Color; 
typedef unsigned long PixelValue;

#if 0
// - The XPixmap class returns Pixmap. 
// - The XDrawer protocol, to
//   which XPixmap conforms, provides the drawOn:X:Y:GC:Caller:, which
//   requires the GC type.  
// - The draw: method of Raster and ZoomRaster is not advertised, since
//   it uses drawOn.
typedef unsigned long Pixmap;     // X.h defines it as an XID
#endif

@protocol _Colormap
- (PixelValue *)map;
- (PixelValue)black;
- (BOOL)setColor: (Color)c ToRed: (double)r Green: (double)g Blue: (double)b;
- (BOOL)setColor: (Color)c ToName: (const char *)colorName;
- (BOOL)setColor: (Color)c ToGrey: (double)g;
@end

@protocol Colormap <Create, _Colormap>
@end

@class Raster;

@protocol Drawer
- drawX: (int)x Y: (int)y;
@end

@protocol _Raster
- drawPointX: (int)x Y: (int)y Color: (Color)c;
- setColormap: (id <Colormap>)c;
- drawSelf;
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- setButton: (int)n Client: c Message: (SEL)sel;
- fillRectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1 Color: (Color)color;
- ellipseX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
      Width: (unsigned)penWidth Color: (Color)c;
- lineX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
   Width: (unsigned)penWidth Color: (Color)c;
- rectangleX0: (int)x0 Y0: (int)y0 X1: (int)x1 Y1: (int)y1
        Width: (unsigned)penWidth Color: (Color)c;
- draw: (id <Drawer>)drawer X: (int)x Y: (int)y;
- erase;
@end

@protocol Raster <ArchivedGeometryWidget, _Raster>
@end

@protocol _ZoomRaster
- increaseZoom;
- decreaseZoom;
- (unsigned)getZoomFactor;
- setZoomFactor: (unsigned)z;
- handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight;
@end

@protocol ZoomRaster <Raster, _ZoomRaster>
@end

@protocol _Pixmap
- setFile: (const char *)filename;
- setRaster: raster;
- createEnd;
- getWidth;
- getHeight;
- drawX: (int)x Y: (int)y;
@end

@protocol Pixmap <_Pixmap, Drawer, Create>
@end

@protocol _CompositeItem
- setTargetId: target;
- setClickSel: (SEL)sel;
- setMoveSel: (SEL)sel;
- setPostMoveSel: (SEL)sel; 
- initiateMoveX: (long)deltaX Y: (long)deltaY;
- moveX: (long)delta_x Y: (long)delta_y;
@end

@protocol CompositeItem <_CompositeItem, SwarmObject>
@end

@protocol _NodeItem
- setX: (int)x Y: (int)y;
- (int)getX;
- (int)getY;
- setString: (const char *)string;
- setFont: (const char *)the_font;
- setString: (const char *)the_text usingFont: (const char *)the_font;
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

@protocol _CanvasItem
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

@protocol CanvasItem <_CanvasItem, SwarmObject>
@end

@protocol _TextItem
- setX: (int) x Y: (int) y;
- setText: (const char *)the_text;
- setText: (const char *)the_text usingFont: (const char *)the_font;
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

