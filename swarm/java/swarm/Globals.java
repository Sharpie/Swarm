package swarm;
import swarm.*;

final public class Globals {
  public static SwarmEnvironment env;

  static {
    try {
      System.loadLibrary ((System.getProperty ("java.vm.name") == "Kaffe")
                          ? "kaffeswarm"
                          : "javaswarm");
    }
    catch (Exception e) {
      System.err.println ("Exception caught: " + e.getMessage ());
    } 
    env = new SwarmEnvironment ();
  }
  
  private Globals () { /* not instantiatiable by user */ 
  }
}
