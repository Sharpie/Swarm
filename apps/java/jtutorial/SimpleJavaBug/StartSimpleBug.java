// StartSimpleBug.java
// The Java SimpleBug application. 

import swarm.Globals;
import swarm.defobj.Zone;

public class StartSimpleBug
{
    // The size of the bug's world and its initial position.
    static int worldXSize = 80;
    static int worldYSize = 80;
    static int xPos = 40;
    static int yPos = 40;

    public static void main (String[] args)
    {
	int i;
	SimpleBug abug;

        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm ("SimpleBug", "2.1", 
			       "bug-swarm@santafe.edu", args);

	// Create an instance of a SimpleBug, abug, and place it 
	// within its world at (xPos, yPos). The bug is created in
	// Swarm's globalZone and is given a "bug id" of 1.
	abug = new SimpleBug(Globals.env.globalZone, worldXSize, worldYSize, 
			     xPos, yPos, 1);

	// Loop our bug through a series of random walks asking it to
	// report its position after each one.
	for (i = 0; i < 100; i++)
	{
	    abug.randomWalk();
	    abug.reportPosition();
	}
    }
}
