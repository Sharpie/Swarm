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
  public ZoneUImpl globalZone;
  public UniformIntegerDistUImpl uniformIntRand;
  public UniformDoubleDistUImpl uniformDblRand;
  public SwarmEnvironment (String args[]) {
    System.out.println ("Global zone!");
    globalZone = new ZoneUImpl (null);
    uniformIntRand = new UniformIntegerDistUImpl (globalZone);
    uniformDblRand = new UniformDoubleDistUImpl (globalZone);
    
    System.out.println ("Init Swarm!");
    initSwarm (args);

  }
}


