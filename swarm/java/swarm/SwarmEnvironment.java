package swarm;
import swarm.random.*;
import swarm.simtoolsgui.*;
import swarm.defobj.*;
import swarm.activity.*;
import swarm.objectbase.*;

public class SwarmEnvironment {
  public Class PrimitiveBoolean,
    PrimitiveCharacter, PrimitiveByte,
    PrimitiveInteger, PrimitiveShort, PrimitiveLong,
    PrimitiveFloat, PrimitiveDouble,
    PrimitiveVoid;
  
  public SwarmEnvironment () {
    super ();
    PrimitiveBoolean = Boolean.TYPE;
    PrimitiveCharacter = Character.TYPE;
    PrimitiveByte = Byte.TYPE;
    PrimitiveInteger = Integer.TYPE;
    PrimitiveShort = Short.TYPE;
    PrimitiveLong = Long.TYPE;
    PrimitiveFloat = Float.TYPE;
    PrimitiveDouble = Double.TYPE;
    PrimitiveVoid = Void.TYPE;
  }
  public boolean booleanp (Class cls) {
    return PrimitiveBoolean.equals (cls);
  }
  public boolean characterp (Class cls) {
    return PrimitiveCharacter.equals (cls);
  }
  public boolean bytep (Class cls) {
    return PrimitiveByte.equals (cls);
  }
  public boolean integerp (Class cls) {
    return PrimitiveInteger.equals (cls);
  }
  public boolean shortp (Class cls) {
    return PrimitiveShort.equals (cls);
  }
  public boolean longp (Class cls) {
    return PrimitiveLong.equals (cls);
  }
  public boolean floatp (Class cls) {
    return PrimitiveFloat.equals (cls);
  }
  public boolean doublep (Class cls) {
    return PrimitiveDouble.equals (cls);
  }
  public boolean voidp (Class cls) {
    return PrimitiveVoid.equals (cls);
  }
  public native void initSwarm (String args[]);
  public native int getCurrentTime ();
  public native SwarmImpl getCurrentSwarm ();
  public native ScheduleImpl getCurrentSchedule ();
  public native SwarmActivityImpl getCurrentSwarmActivity ();
  public native ScheduleActivityImpl getCurrentScheduleActivity();
  public native ActivityImpl getCurrentOwnerActivity ();
  public native ActionImpl getCurrentAction ();
  public native ActivityImpl getCurrentActivity();
  
  public native Object createProbeDisplay (Object anOjbect);
  public native Object createCompleteProbeDisplay (Object anObject);
  public native Object createArchivedProbeDisplay (Object anObject);
  public native Object createArchivedCompleteProbeDisplay (Object anObject);
  public native Object setWindowGeometryRecordName (Object theWidget);
  public native Object setComponentWindowGeometryRecordNameFor (Object obj, Object widget);
  public native Object setComponentWindowGeometryRecordName (Object widget);
  
  public SymbolImpl ControlStateRunning, ControlStateStopped,
    ControlStateStepping, ControlStateQuit, ControlStateNextTime;
  public ProbeLibraryImpl probeLibrary;
  public ProbeDisplayManagerImpl probeDisplayManager;
  
  public ZoneImpl globalZone;
  public UniformIntegerDistImpl uniformIntRand;
  public UniformDoubleDistImpl uniformDblRand;
  public boolean guiFlag;
}


