// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui.h>
#import <gui.h>

#import <objectbase/Arguments.h> // arguments object
#include <misc.h> // xmalloc, stpcpy

#import "ControlPanel.h" //ControlState{Running,Stopped,Stepping,Quit,NextTime}

id <ProbeDisplayManager> probeDisplayManager;

void
initSimtoolsGUI (void)
{
  GUI_INIT (arguments);
  probeDisplayManager = [ProbeDisplayManager create: globalZone];
  
  // various states used in ControlPanel.
  defsymbol (ControlStateRunning);
  defsymbol (ControlStateStopped);
  defsymbol (ControlStateStepping);
  defsymbol (ControlStateQuit);
  defsymbol (ControlStateNextTime);   
}

const char *
buildWindowGeometryRecordName (const char *baseWindowGeometryRecordName,
                               const char *componentName)
{
  if (baseWindowGeometryRecordName)
    {
      char *buf = xmalloc (strlen (baseWindowGeometryRecordName)
                           + 1 + strlen (componentName) + 1);
      
      stpcpy (stpcpy (stpcpy (buf, baseWindowGeometryRecordName), "-"), 
              componentName);
      return buf;
    }
  else
    return NULL;
}

id <ProbeDisplay>
_createProbeDisplay (id obj)
{
  return [probeDisplayManager createProbeDisplayFor: obj];
}

id <ProbeDisplay>
_createCompleteProbeDisplay (id obj)
{
  return [probeDisplayManager createCompleteProbeDisplayFor: obj];
}

static const char *
getKeyForName (id obj, const char *name)
{
  return strcmp (name, "self") == 0 ? [obj name] : name;
}

id <ProbeDisplay>
createArchivedProbeDisplayNamed (id obj, const char *name)
{
  return [probeDisplayManager createArchivedProbeDisplayFor: obj
                              variableName: getKeyForName (obj, name)];
}

id <ProbeDisplay>
createArchivedCompleteProbeDisplayNamed (id obj, const char *name)
{
  return [probeDisplayManager createArchivedCompleteProbeDisplayFor: obj
                       variableName: getKeyForName (obj, name)];
}
