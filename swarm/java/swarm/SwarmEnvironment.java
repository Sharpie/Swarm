package swarm;
import swarm.random.*;
import swarm.simtoolsgui.*;
import swarm.defobj.*;
import swarm.activity.*;
import swarm.objectbase.*;

public class SwarmEnvironment {
  public Symbol ControlStateRunning, ControlStateStopped,
    ControlStateStepping, ControlStateQuit, ControlStateNextTime;
  public Zone globalZone;
  public UniformIntegerDist uniformIntRand;
  public UniformDoubleDist uniformDblRand;
  public ProbeLibrary probeLibrary;
  public ProbeDisplayManager probeDisplayManager;
  public boolean guiFlag;

  public SwarmEnvironment () {
    super ();

    globalZone = new ZoneImpl ();
    uniformIntRand = new UniformIntegerDistImpl ();
    uniformDblRand = new UniformDoubleDistImpl ();
    probeLibrary = new ProbeLibraryImpl ();
    probeDisplayManager = new ProbeDisplayManagerImpl ();
    ControlStateRunning = new SymbolImpl ();
    ControlStateStopped = new SymbolImpl ();
    ControlStateStepping = new SymbolImpl ();
    ControlStateQuit = new SymbolImpl ();
    ControlStateNextTime = new SymbolImpl ();
  }
  public native void initSwarm (String args[]);
  public native int getCurrentTime ();
  public native Swarm getCurrentSwarm ();
  public native Schedule getCurrentSchedule ();
  public native SwarmActivity getCurrentSwarmActivity ();
  public native ScheduleActivity getCurrentScheduleActivity();
  public native Activity getCurrentOwnerActivity ();
  public native Action getCurrentAction ();
  public native Activity getCurrentActivity ();
  
  public native Object createProbeDisplay (Object anOjbect);
  public native Object createCompleteProbeDisplay (Object anObject);
  public native Object createArchivedProbeDisplay (Object anObject);
  public native Object createArchivedCompleteProbeDisplay (Object anObject);
  public native Object setWindowGeometryRecordName (Object theWidget);
  public native Object setComponentWindowGeometryRecordNameFor (Object obj, Object widget);
  public native Object setComponentWindowGeometryRecordName (Object widget);
}


