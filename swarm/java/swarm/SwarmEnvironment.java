package swarm;
import swarm.random.*;
import swarm.simtoolsgui.*;
import swarm.defobj.*;

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
  public ZoneImpl globalZone;
  public UniformIntegerDistImpl uniformIntRand;
  public UniformDoubleDistImpl uniformDblRand;
  public SwarmEnvironment (String args[]) {
    initSwarm (args);

  }
}


