// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.   

#import "internal.h"
#import <defobj.h> // nameToObject
#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/common.h>

#include <misc.h> // strdup

void
tkobjc_dragAndDropTarget (id target, id object)
{
  [globalTkInterp
    eval:
      "drag&drop target %s handler id {%s idReceive}",
    [target getWidgetName],
    [object getObjectName]];
}

static void
dragAndDropTargetArg (id target, id object, int arg)
{
  [globalTkInterp
    eval: "drag&drop target %s handler id {%s idReceive: %d}", 
    [target getWidgetName],
    [object getObjectName],
    arg];
}

static void
oldSetupDragAndDrop (id source, id object)
{
  const char *objectName = [object getObjectName];
  const char *sourceWidgetName = [source getWidgetName];
  
  [globalTkInterp
    eval: "drag&drop source %s config -packagecmd {do_package %s} -sitecmd sitecmd -button 1", 
    sourceWidgetName,
    objectName];
}

static void
newSetupDragAndDrop (id source, id object)
{
  const char *sourceWidgetName = [source getWidgetName];
  const char *objectName = [object getObjectName];

  [globalTkInterp
    eval: "drag&drop source %s -packagecmd {do_package %s %%t} -sitecmd {sitecmd %%s %%t} -button 1", 
    sourceWidgetName,
    objectName];
}

static void
setupDragAndDrop (id source, id object)
{
  if ([globalTkInterp newBLTp])
    newSetupDragAndDrop (source, object);
  else
    oldSetupDragAndDrop (source, object);
}

static void
oldSetupDragAndDropArg (id source, id object, int arg)
{
  const char *sourceWidgetName = [source getWidgetName];
  const char *objectName = [object getObjectName];

  [globalTkInterp
    eval: "drag&drop source %s config -packagecmd {do_package_arg %s %d} -sitecmd sitecmd -button 1",
    sourceWidgetName,
    objectName,
    arg];
}

static void
newSetupDragAndDropArg (id source, id object, int arg)
{
  const char *sourceWidgetName = [source getWidgetName];
  const char *objectName = [object getObjectName];

  [globalTkInterp
    eval: "drag&drop source %s -packagecmd {do_package_arg %s %d %%t} -sitecmd {sitecmd %%s %%t} -button 1", 
    sourceWidgetName,
    objectName,
    arg];
}

void
tkobjc_setupDragAndDropArg (id source, id object, int arg)
{
  if ([globalTkInterp newBLTp])
    newSetupDragAndDropArg (source, object, arg);
  else
    oldSetupDragAndDropArg (source, object, arg);
}

static void
oldSetupHandler (id source)
{
  [globalTkInterp
    eval: "drag&drop source %s handler id send_id", 
    [source getWidgetName]];
}

static void
newSetupHandler (id source)
{
  [globalTkInterp
    eval: "drag&drop source %s handler id {send_id %%i %%w %%v}", 
    [source getWidgetName]];
}

void
tkobjc_setupHandler (id source)
{
  if ([globalTkInterp newBLTp])
    newSetupHandler (source);
  else
    oldSetupHandler (source);
}

void
tkobjc_dragAndDrop (id source, id object)
{
  setupDragAndDrop (source, object);
  tkobjc_setupHandler (source);
}

void
tkobjc_dragAndDropArg (id source, id object, int arg)
{
  dragAndDropTargetArg (source, object, arg);
  tkobjc_setupDragAndDropArg (source, object, arg);
  tkobjc_setupHandler (source);
}

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
tkobjc_configureSpecialBitmap (id widget)
{
  [globalTkInterp
    eval: 
      "%s configure -bitmap special -activeforeground red -foreground red",
    [widget getWidgetName]];
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
        [self getObjectName],
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
        [self getObjectName],
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
    [self getObjectName],
    which,
    [widget getWidgetName],
    [self getWidgetName]];
}

void
tkobjc_ringBell (void)
{
  [globalTkInterp eval: "bell"] ;
}

const char *
tkobjc_dynamicEval (const char *cmd)
{
  [globalTkInterp eval: "%s", cmd];
  return strdup ([globalTkInterp result]);
}

id
tkobjc_drag_and_drop_object (void)
{
  return nameToObject ([[globalTkInterp eval: "gimme $DDOBJ"] result]);
}

void
tkobjc_update (void)
{
  [globalTkInterp eval: "update"];
}

void
tkobjc_releaseAndUpdate (void)
{
#ifndef _WIN32
  [globalTkInterp eval: "foreach w [busy isbusy] {busy release $w} ; update"];
#endif
}

void
tkobjc_updateIdleTasks (int hold)
{
  [globalTkInterp eval:
                    "update idletasks"];

#ifndef _WIN32
  if (hold)
    [globalTkInterp eval:
                  "foreach w [winfo children .] {busy hold $w} ;"
                    "update"];
#endif
}

void
tkobjc_focus (id widget)
{
  [globalTkInterp eval: "focus %s", [widget getWidgetName]];
}

void
tkobjc_makeFrame (id widget)
{
  [globalTkInterp eval: "frame %s", [widget getWidgetName]];
}

void
tkobjc_pack (id widget)
{
  [globalTkInterp eval: "pack %s -fill both -expand true;",
                  [widget getWidgetName]];
}


const char *
tkobjc_createText (id widget, int x, int y, const char *text, const char *font)
{
  return strdup ([[globalTkInterp 
                    eval: 
                      "%s create text %d %d -text \"%s\" %s%s -anchor c", 
                    [widget getWidgetName], x, y, text, 
                    (font ? "-font " : ""),
                    (font ? font : "")]
                   result]);
}
