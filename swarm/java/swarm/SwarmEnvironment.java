package swarm;

public class SwarmEnvironment {
    public static Class globalZone;
    static
    {
        System.out.println("Trying to load lib!\n");
	try {
            globalZone = Class.forName ("swarm.GlobalZone");
	    System.loadLibrary("javaswarm");
	} catch (Exception e) {
	    System.err.println("Exception caught: " + e.getMessage());
	} 
	System.out.println("Lib loaded!\n");
    }
    public native static void initSwarm(String args[]);
}
