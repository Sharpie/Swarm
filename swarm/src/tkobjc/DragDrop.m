// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.   

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>

static const char *
BltDragAndDropConfigTag (void)
{
  const char *version_string = [globalTkInterp getBltVersion];
  int major, minor;
  
  sscanf (version_string, "%d.%d", &major, &minor);
  return (major >= 2 && minor >= 3) ? "" : "config";
}

void
dragAndDropTarget (id target, id object)
{
  [globalTkInterp
    eval:
      "drag&drop target %s handler id {%s idReceive}",
    [target getWidgetName],
    tclObjc_objectToName (object)];
}

void
dragAndDrop (id source, id object)
{
  const char *sourceWidgetName = [source getWidgetName],
    *objectName = tclObjc_objectToName (object);

  [globalTkInterp
    eval: "drag&drop source %s %s -packagecmd {do_package %s} -sitecmd sitecmd -button 1", 
    sourceWidgetName,
    BltDragAndDropConfigTag (),
    objectName];

  [globalTkInterp
    eval: "drag&drop source %s handler id send_id", 
    sourceWidgetName,
    objectName];
}

void
dragAndDropArg (id source, id object, int arg)
{
  const char *sourceWidgetName = [source getWidgetName],
    *objectName = tclObjc_objectToName (object);
  
  [globalTkInterp
    eval: "drag&drop target %s handler id {%s idReceive: %d}", 
    sourceWidgetName,
    objectName,
    arg];

  [globalTkInterp
    eval: "drag&drop source %s %s -packagecmd {do_package_arg %s %d} -sitecmd sitecmd -button 1",
    sourceWidgetName,
    BltDragAndDropConfigTag (),
    objectName,
    arg];
  
  [globalTkInterp
    eval: "drag&drop source %s handler id send_id", 
    sourceWidgetName];
}
