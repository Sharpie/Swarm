// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/control.h>
#import <tkobjc/global.h>
#import <tkobjc/Widget.h>

#import <string.h>

int
doOneEventSync (void)
{
  return Tk_DoOneEvent (TK_ALL_EVENTS);
}

int
doOneEventAsync (void)
{
  return Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT);
}

void
registerCommand (id self, const char *name)
{
  [globalTkInterp registerObject: self withName: name];
}

void
createWindow (id topFrame)
{
  [globalTkInterp eval: "%s create window 0 0 -anchor nw -window %s",
                  [[topFrame getParent] getWidgetName],
                  [topFrame getWidgetName]];
}

void
configureProbeCanvas (id canvas)
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
configureHideButton (id owner, id hideB, id raisedFrame)
{
  [globalTkInterp 
    eval: "%s configure -command {%s markForDrop}",
    [hideB getWidgetName],
    tclObjc_objectToName (owner)];
  [globalTkInterp
    eval: "%s configure -bitmap special -activeforeground red -foreground red", 
    [hideB getWidgetName]];
  
  [globalTkInterp eval: "pack %s -side right -fill both -expand 0",
		  [hideB getWidgetName]];
  
  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand 0",
		  [raisedFrame getWidgetName],
		  [hideB getWidgetName]];
}

void
configureButton3ForCompleteProbeDisplay (id widget,
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
configureWindowEntry (id widget)
{
  [globalTkInterp eval: "bind %s <Enter> {%s configure -fg CornFlowerBlue}",
                  [widget getWidgetName],
                  [widget getWidgetName]];
}


void
configureWindowExit (id widget)
{
  [globalTkInterp eval: "bind %s <Leave> {%s configure -fg blue}",
                  [widget getWidgetName],
                  [widget getWidgetName]];
}

void
setBorderWidth (id frame, int width)
{
  [globalTkInterp eval: "%s configure -bd %d", 
                  [frame getWidgetName],
                  width];
}

void
setRelief (id raisedFrame)
{
  [globalTkInterp eval: "%s configure -relief ridge -borderwidth 3",
    [raisedFrame getWidgetName]] ;
}

void
setAnchorWest (id widget)
{
  [globalTkInterp eval: "%s configure -anchor w",
                  [widget getWidgetName]];
}

void
setAnchorEast (id widget)
{
  [globalTkInterp eval: "%s configure -anchor e",
                  [widget getWidgetName]];
}

void
setColorBlue (id widget)
{
  [globalTkInterp eval: "%s configure -foreground blue",
                  [widget getWidgetName]];
}

void
setWidth (id widget, int width)
{
  [globalTkInterp eval: "%s configure -width %d",
                  [widget getWidgetName],
                  width];
}

void
setText (id widget, const char *str)
{
  [globalTkInterp eval: "%s configure -text %s",
                  [widget getWidgetName],
                  str];
}

void
packFill (id frame)
{
  [globalTkInterp eval: "pack %s -fill both -expand 0",
		  [frame getWidgetName]];
}

void
packFillLeft (id frame, int expandFlag)
{
  [globalTkInterp eval: "pack %s -side left -fill both -expand %d",
		  [frame getWidgetName],
                  expandFlag];
}

void
packForget (id widget)
{
  [globalTkInterp eval:  "pack forget %s ; pack %s -expand true -fill both",
                  [widget getWidgetName],      
                  [widget getWidgetName]];
}

void
assertGeometry (id topFrame)
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
deiconify (id frame)
{
  [globalTkInterp eval: "wm deiconify %s",
                  [frame getWidgetName]];
}

void
withdrawWindow (id topLevel)
{
  [globalTkInterp eval: "wm withdraw %s",
                  [topLevel getWidgetName]];
}

void
releaseAndUpdate (void)
{
  [globalTkInterp eval: "foreach w [busy isbusy] {busy release $w} ; update"];
}

void
updateIdleTasksAndHold (void)
{
  [globalTkInterp eval: 
                    "update idletasks ;"
                  "foreach w [winfo children .] {busy hold $w} ;"
                  "update"] ;
}

void
ringBell (void)
{
  [globalTkInterp eval: "bell"] ;
}

void
normalState (id widget)
{
  [globalTkInterp eval: "%s configure -state normal",
                  [widget getWidgetName]];
}

void
disabledState (id widget)
{
  [globalTkInterp eval: "%s configure -state disabled",
                  [widget getWidgetName]];
}

void
update ()
{
  [globalTkInterp eval: "update"];
}

void
focus (id widget)
{
  [globalTkInterp eval: "focus %s",
                  [widget getWidgetName]];
}

const char *
packageName (id probedObject)
{
  return tclObjc_objectToName (probedObject);
}

const char *
getId (id probedObject)
{
  if ([probedObject respondsTo: @selector(getInstanceName)])
    return [probedObject getInstanceName];
  else
    return [probedObject name];
}

