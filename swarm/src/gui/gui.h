#import <objectbase.h>

@protocol Widget <SwarmObject>
USING
+ createParent: parent;
- pack;
- packFill;
- packBeforeAndFillLeft: widget expand: (BOOL)expandFlag;
- packFillLeft: (BOOL)expandFlag;
- packForgetAndExpand;
- setParent: parent;
- setActiveFlag: (BOOL)activeFlag;
- setBorderWidth: (int)width;
- setWidth: (unsigned)width;
- setHeight: (unsigned)height;
- setWidth: (unsigned)width Height: (unsigned)height;
- setWindowTitle: (const char *)title;
- (const char *)makeWidgetNameFor: widget;
- (const char *)getWidgetName;
- setWidgetNameFromParent: parent;
- setWidgetNameFromParentName: (const char *)parentWidgetName;
- (unsigned)getHeight;
- (unsigned)getWidth;
@end

@protocol ArchivedGeometryWidget <Widget>
- enableDestroyNotification: notificationTarget
         notificationMethod: (SEL)destroyNotificationMethod;
- disableDestroyNotification;
- setWindowGeometryRecordName: (const char *)recordName;
@end

@protocol Frame <ArchivedGeometryWidget>
- enableRelief;
- withdraw;
- deiconify;
- assertGeometry;
- assertPosition;
@end

@protocol Canvas <ArchivedGeometryWidget>
USING
- createEnd;
@end

@protocol ProbeCanvas <Canvas>
- setHorizontalScrollbarFlag: (BOOL)horizontalScrolbarFlag;
@end

@protocol GraphElement <SwarmObject>
USING
- setLabel: (const char *)label;
- addX: (double)x Y: (double)y;
@end

@protocol Graph <ArchivedGeometryWidget>
USING
- setTitle: (const char *)title;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;
- (id <GraphElement>)createElement;
- setScaleModeX: (int)xs Y: (int)ys;
@end

@protocol Histogram <ArchivedGeometryWidget>
USING
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
@end

@protocol Label <Widget>
- setText: (const char *)text;
- anchorEast;
- anchorWest;
- colorBlue;
@end

@protocol CompleteProbeDisplayLabel <Label>
- setProbeDisplay: probeDisplay;
- setProbedObject: probedObject;
- setProbeDisplayManager: probeDisplayManager;
- createEnd;
@end

@protocol Button <Widget>
- setText: (const char *)text;
- setCommand: (const char *)command;
- setButtonTarget: target method: (SEL)method;
@end

@protocol HideButton <Button>
- setSubWidget: subWidget;
- setUser: user;
@end

@protocol SimpleProbeDisplayHideButton <Button>
- setProbeDisplay: probeDisplay;
- setFrame: frame;
@end

@protocol SuperButton <Button>
- setSuperWidget: superWidget;
@end


@protocol Entry <Widget>
- (const char *)getValue;
- setValue: (const char *)value;
@end

@protocol MessageProbeEntry <Entry>
- setResultIdFlag: (BOOL)resultIdFlag;
- setArg: (int)arg;
+ createBegin: aZone;
- createEnd;
@end

@protocol VarProbeEntry <Entry>
- setInteractiveFlag: (BOOL)interactiveFlag;
- setOwner: owner;
- setProbeType: (char)probeType;
- createEnd;
@end

@protocol ButtonPanel <Frame>
- setButtonTarget: target;
- addButtonName: (const char *)n Command: (const char *)c;
- addButtonName: (const char *)n
     actionName: (const char *)action;
@end

typedef unsigned char GUI_Color; 
typedef unsigned long GUI_PixelValue;

@protocol Colormap
- (GUI_PixelValue *)map;
- (GUI_PixelValue)black;
- (BOOL)setColor: (GUI_Color)c ToRed: (double)r Green: (double)g Blue: (double)b;
- (BOOL)setColor: (GUI_Color)c ToName: (const char *)colorName;
- (BOOL)setColor: (GUI_Color)c ToGrey: (double)g;
@end

@protocol Raster <ArchivedGeometryWidget>
- drawPointX: (int)x Y: (int)y Color: (GUI_Color)c;
- setColormap: (id <Colormap>)c;
- drawSelf;
- setWidth: (unsigned)newWidth Height: (unsigned)newHeight;
- setButton: (int)n Client: c Message: (SEL)sel;
- erase;
@end

@protocol ZoomRaster <Raster>
- increaseZoom;
- decreaseZoom;
- (unsigned)getZoomFactor;
- setZoomFactor: (unsigned)z;
- handleConfigureWidth: (unsigned)newWidth Height: (unsigned)newHeight;
@end

@protocol CompositeItem <SwarmObject>
- setTargetId: target;
- setClickSel: (SEL)sel;
- setMoveSel: (SEL)sel;
- setPostMoveSel: (SEL)sel; 
- initiateMoveX: (long)deltaX Y: (long)deltaY;
@end

@protocol NodeItem <CompositeItem>
- setCanvas: canvas;
- (int)getX;
- (int)getY;
- setString: (const char *)string;
- setX: (int)x Y: (int)y;
- setColor: (const char *)color; 
- setBorderColor: (const char *)color;
@end

@protocol OvalNodeItem <NodeItem>
@end

@protocol RectangeNodeItem <NodeItem>
@end

@protocol LinkItem <CompositeItem>
- setFrom: from;
- setTo: to;
@end

#include <tkobjc/common.h>
#define GUI_BEEP() tkobjc_ringBell ()
#define GUI_UPDATE() tkobjc_update ()
#define GUI_UPDATE_IDLE_TASKS_AND_HOLD() tkobjc_updateIdleTasksAndHold ()
#define GUI_RELEASE_AND_UPDATE() tkobjc_releaseAndUpdate () 
#define GUI_DRAG_AND_DROP(source,object) tkobjc_dragAndDrop (source, object)
#define GUI_DRAG_AND_DROP_OBJECT() tkobjc_drag_and_drop_object ()
#define GUI_EVENT_SYNC() tkobjc_doOneEventSync ()
#define GUI_EVENT_ASYNC() tkobjc_doOneEventAsync ()

// for MessageProbeWidget
#define GUI_MAKE_FRAME(widget) tkobjc_makeFrame(widget)
#define GUI_PACK(widget) tkobjc_pack(widget)
// for VarProbeWidget
#define GUI_FOCUS(widget) tkobjc_focus(widget)

#include <tkobjc/global.h>
#define GUI_INIT(argc, argv) initTkObjc (argc, argv)

@class Button;
@class ButtonPanel;
@class ClassDisplayHideButton;
@class ClassDisplayWidget;
@class CompleteProbeDisplayLabel;
@class Frame;
@class Label;
@class MessageProbeEntry;
@class ProbeCanvas;
@class SimpleProbeDisplayHideButton;
@class SuperButton;
@class VarProbeEntry;
@class Widget;

@class XColormap;

@class BLTGraph;
@class Canvas;
@class Histogram;
@class LinkItem;
@class OvalNodeItem;
@class RectangleNodeItem;
@class ZoomRaster;


#define GUI_ButtonRight 3
