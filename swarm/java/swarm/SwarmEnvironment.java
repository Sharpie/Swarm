

public class SwarmEnvironment 
{
   static
    {
	System.out.println("Trying to load lib!\n");
	
	try {
	    System.load("/opt/src/vjojic/Swarm/swarm/java/swarm/libjavaswarm.so");
	} catch (Exception e) { 
	    System.err.println("Exception caught: " + e.getMessage());
	}
	System.out.println("Loaded!\n");
	
    }

    public static void main(String args[])
    {
	SwarmEnvironment se = new SwarmEnvironment();
	initSwarm(args);
    }
    native static void initSwarm(String args[]);
}
