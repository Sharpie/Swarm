// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

package swarm;
import swarm.SwarmEnvironment;
import swarm.SwarmEnvironmentC;
import swarm.SwarmEnvironmentImpl;

final public class Globals {
  public static SwarmEnvironmentCImpl envC;
  public static SwarmEnvironmentImpl env;

  static {
    try {
      System.loadLibrary ((System.getProperty ("java.vm.name") == "Kaffe")
                          ? "kaffeswarm"
                          : "javaswarm");
    } catch (UnsatisfiedLinkError e) {
      System.err.println ("Exception caught: " + e.getMessage ());
    } 
    env = new SwarmEnvironmentImpl ();
    envC = new SwarmEnvironmentCImpl (env);
    envC.createBegin ();
  }
  
  private Globals () { /* not instantiatiable by user */ 
  }
}
