// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

int tkobjc_doOneEventSync (void);
int tkobjc_doOneEventAsync (void);
void tkobjc_registerCommand (id object, const char *name);
void tkobjc_createWindow (id topFrame);
void tkobjc_configureProbeCanvas (id canvas);
void tkobjc_configureHideButton (id owner, id hideB, id raisedFrame);

void tkobjc_bindButton3ForCompleteProbeDisplay (id widget,
                                                id probedObject, 
                                                id theProbeDisplayManager);
void tkobjc_bindButton3ToSpawn (id widget, id self, int focusFlag);
void tkobjc_bindButton3ToArgSpawn (id widget, id self, int which);
void tkobjc_bindButton3ToBeUnhelpful (id widget, id self);
void tkobjc_bindWindowEntry (id widget);
void tkobjc_bindWindowExit (id widget);
void tkobjc_bindReturnToSetValue (id widget, id self);
void tkobjc_bindKeyReleaseReturnToResetColorAndUpdate (id widget);
void tkobjc_bindFocusInToSetSelection (id widget);
void tkobjc_bindFocusOutToClearSelection (id widget);


void tkobjc_configureSpecialBitmap (id widget);
void tkobjc_configureHideBitmap (id widget);
void tkobjc_configureSuperBitmap (id widget);
void tkobjc_configureWidgetToBeep (id widget);
void tkobjc_configureWidgetToDrop (id widget, id owner);
void tkobjc_configureWidgetToPackBeforeAndFillLeftThenDisableAndResize 
(id superB,
 id superClass,
 id self,
 id owner);

void tkobjc_setBorderWidth (id frame, int width);
void tkobjc_setRelief (id frame);
void tkobjc_setAnchorWest (id widget);
void tkobjc_setAnchorEast (id widget);
void tkobjc_setColorBlue (id widget);
void tkobjc_setWidth (id widget, int width);
void tkobjc_setText (id widget, const char *str);
void tkobjc_packToRight (id widget1, id widget2);
void tkobjc_packBeforeAndFillLeft (id widget1, id widget2, int expandFlag);
void tkobjc_packFill (id widget);
void tkobjc_packFillLeft (id widget, int expandFlag);
void tkobjc_packForget (id widget);
void tkobjc_packForgetAndExpand (id widget);
void tkobjc_packForgetArmSuperAndResize (id hideB,
                                         id self,
                                         id mySubClass,
                                         id owner);
void tkobjc_assertGeometry (id topFrame);
void tkobjc_deiconify (id frame);
void tkobjc_withdrawWindow (id topLevel);
void tkobjc_releaseAndUpdate (void);
void tkobjc_updateIdleTasksAndHold (void);
void tkobjc_ringBell (void);
void tkobjc_normalState (id widget);
void tkobjc_disabledState (id widget);
void tkobjc_update (void);
void tkobjc_focus (id widget);
const char *tkobjc_dynamicEval (const char *cmd);
id tkobjc_gimme_drag_and_drop_object (void);

const char *tkobjc_packageName (id probedObject);
const char *tkobjc_getId (id probedObject);

// Items below are for analysis/.

void tkobjc_setupZoomStack (id widget);

void tkobjc_setHistogramBarWidth (id histogram, double step);
void tkobjc_setHistogramXaxisRange (id widget,
                                    double min,
                                    double max,
                                    double step);
void tkobjc_setupHistogramLegend (id histogram);
void tkobjc_setupHistogramActiveOutlierMarker (id histogram);
void tkobjc_histogramActiveItemInfo (id histogram);
void tkobjc_setHistogramActiveOutlierText (id histogram,
                                           int outliers,
                                           int count);



