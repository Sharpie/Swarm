

public class SwarmEnvironment 
{
   static Class globalZone;
   static
    {
	System.out.println("Trying to load lib!\n");
	try {
	    System.loadLibrary("javaswarm");
	    globalZone = Class.forName ("GlobalZone");
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
