package swarm;
import swarm.random.*;
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
  public native static void initSwarm (GlobalZone globalZone,
				       UniformIntegerDistU uniformIntRand,
				       UniformDoubleDistU uniformDblRand,
				       String args[]);
  public GlobalZone globalZone;
  public UniformIntegerDistU uniformIntRand;
  public UniformDoubleDistU uniformDblRand;
  public SwarmEnvironment(String args[]) {
    System.out.println ("Global zone!");
    globalZone = new GlobalZone ();
    uniformIntRand = new UniformIntegerDistU();
    uniformDblRand = new UniformDoubleDistU();
    System.out.println ("Init Swarm!");
    initSwarm (globalZone, uniformIntRand, uniformDblRand, args);

  }
}
