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

id <ProbeDisplayManager> probeDisplayManager;
int swarmGUIMode;
id applicationName, applicationMode;
void printHelp();

static void
setApplicationValue (id value, const char *ptr)
{
  const char *appStr = ptr;
  
  while (*ptr)
    {
      if (*ptr == '/')
        appStr = ptr + 1;
      ptr++;
    }
  [value setC: appStr];
}

void
initSwarm (int argc, const char **argv)
{
  int i;

  initModule(activity);
  initProbing();

  swarmGUIMode = 1;
  applicationName = [String create: globalZone setC: ""];
  applicationMode = [String create: globalZone setC: ""];
  setApplicationValue (applicationName, argv[0]);
  setApplicationValue (applicationMode, "default");

  for (i = 1; i < argc; i++)
    {
      if (!strcmp (argv[i],"-help"))
        printHelp ();
      else if (!strcmp (argv[i],"-batchmode"))
      swarmGUIMode = 0;
      else if (!strncmp (argv[i], "-a", 2) && (i + 1 < argc))
        setApplicationValue (applicationMode, argv[i+1]);
    }
  
  archiver = [Archiver ensure: globalZone];

  initRandom (argc, argv);

  GUI_INIT (argc, argv);
  if (swarmGUIMode)
    probeDisplayManager = [ProbeDisplayManager create: globalZone];
  
  // various states used in ControlPanel.
  defsymbol(ControlStateRunning);
  defsymbol(ControlStateStopped);
  defsymbol(ControlStateStepping);
  defsymbol(ControlStateQuit);
  defsymbol(ControlStateNextTime);
}

void
printHelp()
{
  (void)fprintf (stdout, "Swarm.  Copyright (C) 1996-1998 Santa Fe Institute\n");
  (void)fprintf (stdout, "For more info, see:\n"
	 "http://www.santafe.edu/projects/swarm\n\n");
  (void)fprintf (stdout, "Supported command line flags are:\n\n");
  (void)fprintf (stdout, "\t  -appMode: Change the mode of the application\n");
  (void)fprintf (stdout, "\t-batchmode:  Run without a GUI\n");
  (void)fprintf (stdout, "\t -varySeed:  Change RandomSeed for each run\n");
  exit (-1);
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
