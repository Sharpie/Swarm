package swarm;

public class SwarmEnvironment 
{
   static Class globalZone;
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

    public static void main(String args[])
    {
	initSwarm(args);
    }
    native static void initSwarm(String args[]);
}
