// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

int doOneEventSync (void);
int doOneEventAsync (void);
void registerCommand (id object, const char *name);
void createWindow (id topFrame);
void configureProbeCanvas (id canvas);
void configureHideButton (id owner, id hideB, id raisedFrame);
void configureButton3ForCompleteProbeDisplay (id widget,
                                              id probedObject, 
                                              id theProbeDisplayManager);
void configureWindowEntry (id widget);
void configureWindowExit (id widget);
void setBorderWidth (id frame, int width);
void setRelief (id frame);
void setAnchorWest (id widget);
void setAnchorEast (id widget);
void setColorBlue (id widget);
void setWidth (id widget, int width);
void setText (id widget, const char *str);
void packFill (id frame);
void packFillLeft (id frame, int expandFlag);
void packForget (id widget);
void assertGeometry (id topFrame);
void deiconify (id frame);
void withdrawWindow (id topLevel);
void releaseAndUpdate (void);
void updateIdleTasksAndHold (void);
void ringBell (void);
void normalState (id widget);
void disabledState (id widget);
void update (void);
void focus (id widget);

const char *packageName (id probedObject);
const char *getId (id probedObject);

