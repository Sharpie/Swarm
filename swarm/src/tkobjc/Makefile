SWARMHOME=../..
LIBNAME=tkobjc

ifeq ($(MACHTYPE),i386-pc-cygwin32)
BACKEND_H=win32dib.h
BACKEND_O=win32dib.o
else
BACKEND_H=XPixmap.h
BACKEND_O=XPixmap.o
endif

OBJECTS=Graph.o Frame.o Widget.o Button.o TkExtra.o ButtonPanel.o Raster.o ZoomRaster.o Colormap.o Entry.o Label.o Form.o Histogram.o CheckButton.o InputWidget.o tkobjc.o Canvas.o CanvasItem.o CompositeItem.o Circle.o Line.o Rectangle.o LinkItem.o NodeItem.o OvalNodeItem.o RectangleNodeItem.o TextItem.o WindowGeometryRecord.o ArchivedGeometryWidget.o ClassDisplayHideButton.o ClassDisplayLabel.o VarProbeLabel.o SimpleProbeDisplayHideButton.o SuperButton.o MessageProbeEntry.o VarProbeEntry.o CompleteProbeDisplayLabel.o ProbeCanvas.o common.o internal.o $(BACKEND_O)

HEADERS=Graph.h Button.h ButtonPanel.h Entry.h Form.h Frame.h Histogram.h Label.h Raster.h TkExtra.h Widget.h Colormap.h XDrawer.h ZoomRaster.h global.h CheckButton.h InputWidget.h Canvas.h CanvasItem.h CompositeItem.h Circle.h Line.h Rectangle.h LinkItem.h NodeItem.h OvalNodeItem.h RectangleNodeItem.h TextItem.h WindowGeometryRecord.h ArchivedGeometryWidget.h ClassDisplayHideButton.h ClassDisplayLabel.h VarProbeLabel.h SimpleProbeDisplayHideButton.h SuperButton.h MessageProbeEntry.h VarProbeEntry.h CompleteProbeDisplayLabel.h ProbeCanvas.h common.h internal.h $(BACKEND_H)

OTHERCLEAN=testFrame testGraph testHistogram testRaster testFrame.o testGraph.o testHistogram.o testRaster.o

include $(SWARMHOME)/Makefile.lib

testFrame: testFrame.o $(LIBRARY)
	$(CC) $(CFLAGS) $(LDFLAGS) -o testFrame testFrame.o -ltkobjc -lsimtools $(LIBS)
testHistogram: testHistogram.o $(LIBRARY)
	$(CC) $(CFLAGS) $(LDFLAGS) -o testHistogram testHistogram.o -ltkobjc -lsimtools $(LIBS)
testRaster: testRaster.o $(LIBRARY)
	$(CC) $(CFLAGS) $(LDFLAGS) -o testRaster testRaster.o -ltkobjc -lsimtools $(LIBS)
testGraph: testGraph.o $(LIBRARY)
	$(CC) $(CFLAGS) $(LDFLAGS) -o testGraph testGraph.o -ltkobjc -lsimtools $(LIBS)

TkExtra.o: TkExtra.m TkExtra.h
Widget.o: Widget.m Widget.h
Graph.o: Graph.m Graph.h global.h Widget.h
Frame.o: Frame.m Frame.h global.h Widget.h
Button.o: Button.m Button.h global.h Widget.h
ButtonPanel.o: ButtonPanel.m ButtonPanel.h Frame.h global.h Widget.h
Raster.o: Raster.m Raster.h global.h Widget.h Colormap.h XDrawer.h
ZoomRaster.o: ZoomRaster.m ZoomRaster.h Raster.h global.h Widget.h XDrawer.h
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
WindowGeometryRecord.o: WindowGeometryRecord.m WindowGeometryRecord.h
ArchivedGeometryWidget.o: ArchivedGeometryWidget.m ArchivedGeometryWidget.h WindowGeometryRecord.h
ClassDisplayHideButton.o: ClassDisplayHideButton.m ClassDisplayHideButton.h
ClassDisplayLabel.o: ClassDisplayLabel.m ClassDisplayLabel.h
VarProbeLabel.o: VarProbeLabel.m VarProbeLabel.h
SimpleProbeDisplayHideButton.o: SimpleProbeDisplayHideButton.m SimpleProbeDisplayHideButton.h
SuperButton.o: SuperButton.m SuperButton.h
MessageProbeEntry.o: MessageProbeEntry.m MessageProbeEntry.h
VarProbeEntry.o: VarProbeEntry.m VarProbeEntry.h
ProbeCanvas.o: ProbeCanvas.m ProbeCanvas.h
common.o: common.m common.h

internal.o: internal.m internal.h
	$(CC) -c $(CFLAGS) -Itk $<

tkobjc.o: analysis_tcl.x simtools_tcl.x comm_tcl.x tkbusy_tcl.x

%_tcl.x: %.tcl
	(echo 'const char *$(basename $<)_tcl = "' ; cat $< | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' ; echo '";' ) > $@
