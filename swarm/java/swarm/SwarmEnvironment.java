package swarm;
import swarm.random.*;
import swarm.simtoolsgui.*;
import swarm.defobj.*;
import swarm.activity.*;
import swarm.objectbase.*;

public class SwarmEnvironment {
  static {
    System.out.println ("Trying to load lib!\n");
    try {
      System.loadLibrary ("javaswarm");
    }
    catch (Exception e) {
      System.err.println ("Exception caught: " + e.getMessage ());
    } 
    System.out.println ("Lib loaded!\n");
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

  public ZoneImpl globalZone;
  public UniformIntegerDistImpl uniformIntRand;
  public UniformDoubleDistImpl uniformDblRand;
  public SwarmEnvironment (String args[]) {
    initSwarm (args);

  }
}


