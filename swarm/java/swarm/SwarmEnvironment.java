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
      System.err.println ("Exception caught: " + e.getMessage());
    } 
    System.out.println ("Lib loaded!\n");
  }
  public native void initSwarm (String args[]);
  public ZoneU globalZone;
  public UniformIntegerDistU uniformIntRand;
  public UniformDoubleDistU uniformDblRand;
  public SwarmEnvironment(String args[]) {
    System.out.println ("Global zone!");
    globalZone = new ZoneU ();
    uniformIntRand = new UniformIntegerDistU();
    uniformDblRand = new UniformDoubleDistU();
    
    System.out.println ("Init Swarm!");
    initSwarm (args);

  }
}


