// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/control.h>
#import <tkobjc/global.h>
#import <tkobjc/Widget.h>

#import <string.h>

int
tkobjc_doOneEventSync (void)
{
  return Tk_DoOneEvent (TK_ALL_EVENTS);
}

int
tkobjc_doOneEventAsync (void)
{
  return Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT);
}

void
tkobjc_registerCommand (id self, const char *name)
{
  [globalTkInterp registerObject: self withName: name];
}

void
tkobjc_createWindow (id topFrame)
{
  [globalTkInterp eval: "%s create window 0 0 -anchor nw -window %s",
                  [[topFrame getParent] getWidgetName],
                  [topFrame getWidgetName]];
}

void
tkobjc_configureProbeCanvas (id canvas)
{
  id c_Frame = [canvas getParent];

  [globalTkInterp eval: 
   "%s configure -width 10 -height 10 -yscrollcommand {%s.yscroll set} ; \
    scrollbar %s.yscroll -orient vertical -command {%s yview} ; \
    pack %s.yscroll -side right -fill y ; \
    pack %s -side left -fill both  -expand true",
    [canvas getWidgetName],
    [c_Frame getWidgetName],
    [c_Frame getWidgetName],
    [canvas getWidgetName],
    [c_Frame getWidgetName],
    [canvas getWidgetName]];
}

void
tkobjc_configureHideBitmap (id widget)
{
  [globalTkInterp
    eval: "%s configure -bitmap special -activeforeground red -foreground red", 
    [widget getWidgetName]];
}

void
tkobjc_configureSpecialBitmap (id widget)
{
  [globalTkInterp
    eval: 
      "%s configure -bitmap special -activeforeground red -foreground red",
    [widget getWidgetName]];
}

void
tkobjc_configureSuperBitmap (id widget)
{
  [globalTkInterp
    eval:
      "%s configure -bitmap super -activeforeground forestgreen -foreground forestgreen", 
    [widget getWidgetName]];
}

void
tkobjc_configureHideButton (id owner, id hideB, id raisedFrame)
{
  [globalTkInterp 
    eval: "%s configure -command {%s markForDrop}",
    [hideB getWidgetName],
    tclObjc_objectToName (owner)];

  tkobjc_configureHideBitmap (hideB);
 
  [globalTkInterp eval: "pack %s -side right -fill both -expand 0",
		  [hideB getWidgetName]];
  
  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand 0",
		  [raisedFrame getWidgetName],
		  [hideB getWidgetName]];
}

void
tkobjc_configureWidgetToBeep (id widget)
{
  [globalTkInterp
    eval: 
      "%s configure -command { bell }",
    [widget getWidgetName]];
}

void
tkobjc_configureWidgetToDrop (id widget, id owner)
{
  [globalTkInterp 
    eval:
      "%s configure -command {%s drop}",
    [widget getWidgetName],
    tclObjc_objectToName (owner)];
}

void
tkobjc_configureWidgetToPackBeforeAndFillLeftThenDisableAndResize
(id superB,
 id superClass,
 id self,
 id owner)
{
  [globalTkInterp 
    eval:
      "%s configure -command { pack %s -before %s -fill both -expand 1; %s configure -state disabled; %s do_resize}",
    [superB getWidgetName],
    [superClass getWidgetName],
    [self getWidgetName],
    [superB getWidgetName],
    tclObjc_objectToName (owner)];
}


void
tkobjc_bindButton3ToSpawn (id widget, id self, int focusFlag)
{
  const char *widgetName = [widget getWidgetName];

  if (focusFlag)
    {
      [globalTkInterp
        eval:
          "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;"
        "update ; %s Spawn ; %s configure -highlightcolor black ;"
        "update ; focus %s ; update } ;",
        widgetName,
        widgetName,
        widgetName,
        tclObjc_objectToName (self),
        widgetName,
        widgetName];
    }
  else
    {
      [globalTkInterp
        eval:
          "bind %s <Button-3> {focus %s; %s configure -highlightcolor red;"
        "update;"
        "%s Spawn;"
        "%s configure -highlightcolor black;"
        "update};",
        widgetName,
        widgetName,
        widgetName,
        tclObjc_objectToName (self),
        widgetName];
    }
}

void
tkobjc_bindButton3ToArgSpawn (id widget, id self, int which)
{
  [globalTkInterp
    eval:
      "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;"
    "update ; %s argSpawn: %d ; %s configure -highlightcolor black ;"
    "update ; focus %s ; update } ;",
    [widget getWidgetName],
    [widget getWidgetName],
    [widget getWidgetName], 
    tclObjc_objectToName (self),
    which,
    [widget getWidgetName],
    [self getWidgetName]];
}


void
tkobjc_bindButton3ToBeUnhelpful (id widget, id self)
{
  const char *widgetName = [widget getWidgetName];

  if (self != nil)
    [globalTkInterp
      eval:
        "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;"
      "update ; bell ; update ; %s configure -highlightcolor black ;"
      "update ; focus %s ; update} ;",
      widgetName, widgetName, widgetName, widgetName,
      [self getWidgetName]];
  else
    [globalTkInterp
      eval:
        "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;"
      "update ;"
      "bell ; update ; "
      "%s configure -highlightcolor black ;"
      "update} ;",
      widgetName, widgetName, widgetName, widgetName];
}

void
tkobjc_bindReturnToSetValue (id widget, id self)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval:
      "bind %s <Return> {%s configure -highlightcolor red ;"
    "update ;"
    "%s setValue}",
    widgetName, widgetName,
    tclObjc_objectToName (self)];
}

void
tkobjc_bindKeyReleaseReturnToResetColorAndUpdate (id widget)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval: "bind %s <KeyRelease-Return> {%s configure -highlightcolor black ;"
    "update };", 
    widgetName,
    widgetName];
}

void
tkobjc_bindFocusInToSetSelection (id widget)
{
  const char *widgetName = [widget getWidgetName];

  [globalTkInterp
    eval: "bind %s <FocusIn> {%s selection range 0 end}",
    widgetName,
    widgetName];
}

void
tkobjc_bindFocusOutToClearSelection (id widget)
{
  const char *widgetName = [widget getWidgetName];
  [globalTkInterp
    eval: "bind %s <FocusOut> {%s selection clear}",
    widgetName,
    widgetName];
}
 
void
tkobjc_bindButton3ForCompleteProbeDisplay (id widget,
                                           id probedObject,
                                           id theProbeDisplayManager)
{
  // have to make a private copy of the return for objectToName.
  const char *pdmName = tclObjc_objectToName (theProbeDisplayManager);
  char pdmNameCopy[strlen (pdmName) + 1];

  strcpy (pdmNameCopy, pdmName);
  
  [globalTkInterp 
    eval: 
      "bind %s <ButtonPress-3> {%s createCompleteProbeDisplayFor: %s}",
    [widget getWidgetName],
    pdmNameCopy,
    tclObjc_objectToName (probedObject)];
}

void
tkobjc_bindWindowEntry (id widget)
{
  [globalTkInterp eval: "bind %s <Enter> {%s configure -fg CornFlowerBlue}",
                  [widget getWidgetName],
                  [widget getWidgetName]];
}


void
tkobjc_bindWindowExit (id widget)
{
  [globalTkInterp eval: "bind %s <Leave> {%s configure -fg blue}",
                  [widget getWidgetName],
                  [widget getWidgetName]];
}

void
tkobjc_setBorderWidth (id frame, int width)
{
  [globalTkInterp eval: "%s configure -bd %d", 
                  [frame getWidgetName],
                  width];
}

void
tkobjc_setRelief (id raisedFrame)
{
  [globalTkInterp eval: "%s configure -relief ridge -borderwidth 3",
    [raisedFrame getWidgetName]] ;
}

void
tkobjc_setAnchorWest (id widget)
{
  [globalTkInterp eval: "%s configure -anchor w",
                  [widget getWidgetName]];
}

void
tkobjc_setAnchorEast (id widget)
{
  [globalTkInterp eval: "%s configure -anchor e",
                  [widget getWidgetName]];
}

void
tkobjc_setColorBlue (id widget)
{
  [globalTkInterp eval: "%s configure -foreground blue",
                  [widget getWidgetName]];
}

void
tkobjc_setWidth (id widget, int width)
{
  [globalTkInterp eval: "%s configure -width %d",
                  [widget getWidgetName],
                  width];
}

void
tkobjc_setText (id widget, const char *str)
{
  [globalTkInterp eval: "%s configure -text %s",
                  [widget getWidgetName],
                  str];
}

void
tkobjc_packToRight (id widget1, id widget2)
{
  [globalTkInterp eval: "pack %s %s -side right",
                  [widget1 getWidgetName],
                  [widget2 getWidgetName]];
}

void
tkobjc_packBeforeAndFillLeft (id widget1, id widget2, int expandFlag)
{
  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand %d",
		  [widget1 getWidgetName],
		  [widget2 getWidgetName],
                  expandFlag];
}

void
tkobjc_packFill (id widget)
{
  [globalTkInterp eval: "pack %s -fill both -expand 0",
		  [widget getWidgetName]];
}

void
tkobjc_packFillLeft (id widget, int expandFlag)
{
  [globalTkInterp eval: "pack %s -side left -fill both -expand %d",
		  [widget getWidgetName],
                  expandFlag];
}

void
tkobjc_packForget (id widget)
{
  [globalTkInterp eval: "pack forget %s",
                  [widget getWidgetName]];
}

void
tkobjc_packForgetAndExpand (id widget)
{
  tkobjc_packForget (widget);
  [globalTkInterp eval:  "pack %s -expand true -fill both",
                  [widget getWidgetName]];
}

void
tkobjc_packForgetArmSuperAndResize (id hideB, id self, id mySubClass, id owner)
{
  [globalTkInterp 
    eval: 
      "%s configure -command {pack forget %s; %s armSuperButton; %s do_resize}",
    [hideB getWidgetName],
    [self getWidgetName],
    tclObjc_objectToName (mySubClass),
    tclObjc_objectToName (owner)];
}

void
tkobjc_assertGeometry (id topFrame)
{
  id canvas = [topFrame getParent];
  
  [globalTkInterp eval:
                    "tkwait visibility %s ;"
                  "set width [winfo width %s] ;"
                  "set height [winfo height %s] ;"
                  "%s configure -scrollregion [list 0 0 $width $height] ;"
                  "if {$height > 500} {set height 500} ;"
                  "%s configure -width $width -height $height",
                  [topFrame getWidgetName],
                  [topFrame getWidgetName],
                  [topFrame getWidgetName],
                  [canvas getWidgetName],
                  [canvas getWidgetName]];
}

void
tkobjc_deiconify (id frame)
{
  [globalTkInterp eval: "wm deiconify %s",
                  [frame getWidgetName]];
}

void
tkobjc_withdrawWindow (id topLevel)
{
  [globalTkInterp eval: "wm withdraw %s",
                  [topLevel getWidgetName]];
}

void
tkobjc_releaseAndUpdate (void)
{
  [globalTkInterp eval: "foreach w [busy isbusy] {busy release $w} ; update"];
}

void
tkobjc_updateIdleTasksAndHold (void)
{
  [globalTkInterp eval: 
                    "update idletasks ;"
                  "foreach w [winfo children .] {busy hold $w} ;"
                  "update"] ;
}

void
tkobjc_ringBell (void)
{
  [globalTkInterp eval: "bell"] ;
}

void
tkobjc_normalState (id widget)
{
  [globalTkInterp eval: "%s configure -state normal",
                  [widget getWidgetName]];
}

void
tkobjc_disabledState (id widget)
{
  [globalTkInterp eval: "%s configure -state disabled",
                  [widget getWidgetName]];
}

void
tkobjc_update ()
{
  [globalTkInterp eval: "update"];
}

void
tkobjc_focus (id widget)
{
  [globalTkInterp eval: "focus %s",
                  [widget getWidgetName]];
}

const char *
tkobjc_dynamicEval (const char *cmd)
{
  [globalTkInterp eval: "%s", cmd];
  return strdup ([globalTkInterp result]);
}

id
tkobjc_gimme_drag_and_drop_object (void)
{
  return tclObjc_nameToObject ([[globalTkInterp eval: "gimme $DDOBJ"] result]);
}

const char *
tkobjc_packageName (id probedObject)
{
  return tclObjc_objectToName (probedObject);
}

const char *
tkobjc_getId (id probedObject)
{
  if ([probedObject respondsTo: @selector (getInstanceName)])
    return [probedObject getInstanceName];
  else
    return [probedObject name];
}

