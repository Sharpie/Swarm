// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdlib.h>
#import <time.h>
#import <string.h>

#import <objectbase/probing.h>
#import <simtools.h>
#import <activity.h>

#import <simtools.h>
#import <simtools/Archiver.h>
#import <gui.h>

#import <objectbase/Arguments.h>

id <ProbeDisplayManager> probeDisplayManager;
int swarmGUIMode;
id arguments;

void
initSwarm (int argc, const char **argv)
{
  swarmGUIMode = 1;

  initModule (activity);
  initProbing ();

  arguments = [Arguments createArgc: argc Argv: argv];
  if ([arguments getBatchModeFlag])
    swarmGUIMode = 0;

  archiver = [Archiver ensure: globalZone];

  initRandom (arguments);

  GUI_INIT (arguments);
  if (swarmGUIMode)
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
      char *buf = malloc (strlen (baseWindowGeometryRecordName)
                          + 1
                          + strlen (componentName)
                          + 1);

      strcpy (buf, baseWindowGeometryRecordName);
      strcat (buf, "-");
      strcat (buf, componentName);
      return buf;
    }
  else
    return NULL;
}

void _createProbeDisplay (id obj)
{
  [probeDisplayManager createProbeDisplayFor: obj];
}

void _createCompleteProbeDisplay (id obj)
{
  [probeDisplayManager createCompleteProbeDisplayFor: obj];
}

static const char *
getKeyForName (id obj, const char *name)
{
  return strcmp (name, "self") == 0 ? [obj name] : name;
}

void createArchivedProbeDisplayNamed (id obj, const char *name)
{
  [probeDisplayManager createArchivedProbeDisplayFor: obj
                       variableName: getKeyForName (obj, name)];
}

void createArchivedCompleteProbeDisplayNamed (id obj, const char *name)
{
  [probeDisplayManager createArchivedCompleteProbeDisplayFor: obj
                       variableName: getKeyForName (obj, name)];
}
