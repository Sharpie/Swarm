

public class SwarmEnvironment 
{
   static
    {
	System.out.println("Trying to load lib!\n");
	try {
	    System.loadLibrary("javaswarm");
	} catch (Exception e) { 
	    System.err.println("Exception caught: " + e.getMessage());
	}
	System.out.println("Loaded!\n");
	
    }

    public static void main(String args[])
    {
	initSwarm(args);
    }
    native static void initSwarm(String args[]);
}
