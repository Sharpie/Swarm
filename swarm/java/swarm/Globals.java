package swarm;
import swarm.*;

final public class Globals {

    public static SwarmEnvironment env;
  
    static {
        System.out.println ("Trying to load lib!\n");
        try {
            System.loadLibrary ("javaswarm");
        }
        catch (Exception e) {
            System.err.println ("Exception caught: " + e.getMessage ());
        } 
        System.out.println ("Lib loaded!\n");
        env = new SwarmEnvironment();
    }
    
    private Globals() { /* not instantiatiable by user */ 
        
    }
}
