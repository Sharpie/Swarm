// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

int doOneEventSync (void);
int doOneEventAsync (void);

void registerCommand (id object, const char *name);
void configureProbeCanvas (id canvas);
void configureHideButton (id owner, id hideB, id raisedFrame);
void configureButton3ForCompleteProbeDisplay (id widget, id probedObject);
void setBorderWidth (id frame, int width);
void setRelief (id frame);
void setAnchorWest (id widget);
void setColorBlue (id widget);
void createWindow (id topFrame);
void configureWindowEntry (id widget);
void packFillLeft (id frame, int expandFlag);
void assertGeometry (id topFrame);
void withdrawWindow (id topLevel);
const char *packageName (id probedObject);
const char *getId (id probedObject);

