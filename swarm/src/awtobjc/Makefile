SWARMHOME=../..

LIBNAME=awtobjc

GUIOBJECTS=JavaObject.o JavaControlPanel.o JavaEnv.o JavaInput.o JavaMutex.o JavaItem.o

OBJECTS=Colormap.o Graph.o Button.o ButtonPanel.o Canvas.o CanvasItem.o CheckButton.o Circle.o CompositeItem.o Entry.o Frame.o Histogram.o InputWidget.o Label.o Line.o LinkItem.o NodeItem.o OvalNodeItem.o Raster.o Rectangle.o RectangleNodeItem.o TextItem.o Widget.o ZoomRaster.o ClassDisplayLabel.o VarProbeEntry.o CompleteProbeDisplayLabel.o ClassDisplayHideButton.o SimpleProbeDisplayHideButton.o MessageProbeEntry.o ProbeCanvas.o SuperButton.o VarProbeLabel.o ArchivedGeometryWidget.o WindowGeometryRecord.o awtobjc.o common.o $(GUIOBJECTS)

HEADERS=Colormap.h Graph.h Canvas.h CanvasItem.h Entry.h Frame.h Histogram.h InputWidget.h JavaObject.h JavaEnv.h JavaInput.h Label.h LinkItem.h NodeItem.h OvalNodeItem.h Raster.h RectangleNodeItem.h Widget.h ZoomRaster.h global.h CompositeItem.h Drawer.h Button.h ButtonPanel.h Circle.h JavaControlPanel.h CheckButton.h JavaMutex.h Line.h Rectangle.h TextItem.h ClassDisplayLabel.h VarProbeEntry.h CompleteProbeDisplayLabel.h ClassDisplayHideButton.h SimpleProbeDisplayHideButton.h MessageProbeEntry.h ProbeCanvas.h SuperButton.h VarProbeLabel.h ArchivedGeometryWidget.h WindowGeometryRecord.h Java.h common.h JavaItem.h

include $(SWARMHOME)/Makefile.lib

# cp javai.a $(SWARMHOME)/lib

TkExtra.o: TkExtra.m TkExtra.h
Widget.o: Widget.m Widget.h
Graph.o: Graph.m Graph.h global.h Widget.h
Frame.o: Frame.m Frame.h global.h Widget.h
Button.o: Button.m Button.h global.h Widget.h
ButtonPanel.o: ButtonPanel.m ButtonPanel.h Frame.h global.h Widget.h
Raster.o: Raster.m Raster.h global.h Widget.h
ZoomRaster.o: ZoomRaster.m ZoomRaster.h Raster.h global.h Widget.h
Label.o: Widget.h global.h Label.m Label.h
Form.o: Widget.h Label.h Entry.h Frame.h Form.h Form.m
Histogram.o: Histogram.h Widget.h Histogram.m
Colormap.o: Colormap.m Colormap.h
XPixmap.o: XPixmap.m XPixmap.h XDrawer.h
Entry.o: Widget.h global.h Entry.m Entry.h InputWidget.h
CheckButton.o: CheckButton.m CheckButton.h InputWidget.h global.h
InputWidget.o: InputWidget.m InputWidget.h global.h Widget.h
Canvas.o: Canvas.m Canvas.h Widget.h
CanvasItem.o: CanvasItem.m CanvasItem.h 
CompositeItem.o: CompositeItem.m CompositeItem.h 
Circle.o: Circle.m Circle.h CanvasItem.h 
Line.o: Line.m Line.h  CanvasItem.h 
Rectangle.o: Rectangle.m Rectangle.h  CanvasItem.h 
NodeItem.o: NodeItem.m NodeItem.h CompositeItem.h
OvalNodeItem.o: OvalNodeItem.m OvalNodeItem.h NodeItem.h
RectangleNodeItem.o: RectangleNodeItem.m RectangleNodeItem.h NodeItem.h
LinkItem.o: LinkItem.m LinkItem.h CanvasItem.h NodeItem.h
TextItem.o: TextItem.m TextItem.h CanvasItem.h

awtobjc.o: awtobjc.m

install-lib:
	$(MAKE) -C java
