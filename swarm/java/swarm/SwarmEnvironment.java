package swarm;

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
  public native static void initSwarm(String args[]);
  public GlobalZone globalZone;
  public SwarmEnvironment (String args[]) {
    globalZone = new GlobalZone ();
    initSwarm (args);
  }
}
