// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <simtoolsgui.h>
#import <gui.h>

#import <defobj.h> // arguments
#include <misc.h> // stpcpy

externvardef id <ProbeDisplayManager> probeDisplayManager;

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
      char *buf = [scratchZone alloc:
                                 (strlen (baseWindowGeometryRecordName)
                                  + 1 + strlen (componentName) + 1)];
      
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
