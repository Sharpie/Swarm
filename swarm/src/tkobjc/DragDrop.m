// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.   

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>

void
dragAndDropTarget (id target, id object)
{
  [globalTkInterp
    eval:
      "drag&drop target %s handler id {%s idReceive}",
    [target getWidgetName],
    tclObjc_objectToName (object)];
}

static void
dragAndDropTargetArg (id target, id object, int arg)
{
  [globalTkInterp
    eval: "drag&drop target %s handler id {%s idReceive: %d}", 
    [target getWidgetName],
    tclObjc_objectToName (object),
    arg];
}

static void
oldSetupDragAndDrop (id source, id object)
{
  const char *objectName = tclObjc_objectToName (object);
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
  const char *objectName = tclObjc_objectToName (object);

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
  const char *objectName = tclObjc_objectToName (object);

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
  const char *objectName = tclObjc_objectToName (object);

  [globalTkInterp
    eval: "drag&drop source %s -packagecmd {do_package_arg %s %d %%t} -sitecmd {sitecmd %%s %%t} -button 1", 
    sourceWidgetName,
    objectName,
    arg];
}

void
setupDragAndDropArg (id source, id object, int arg)
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
setupHandler (id source)
{
  if ([globalTkInterp newBLTp])
    newSetupHandler (source);
  else
    oldSetupHandler (source);
}

void
dragAndDrop (id source, id object)
{
  setupDragAndDrop (source, object);
  setupHandler (source);
}

void
dragAndDropArg (id source, id object, int arg)
{
  dragAndDropTargetArg (source, object, arg);
  setupDragAndDropArg (source, object, arg);
  setupHandler (source);
}

