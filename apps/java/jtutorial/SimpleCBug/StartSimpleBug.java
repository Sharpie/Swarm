// StartSimpleBug.java
// The Java SimpleBug application. 

import swarm.Globals;

public class StartSimpleBug
{
    public static void main (String[] args)
    {
	// The size of the bug's world and its initial position.
	int worldXSize = 80;
	int worldYSize = 80;
	int xPos = 40;
	int yPos = 40;

	int i;

        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm ("SimpleBug", "2.1", 
			       "bug-swarm@santafe.edu", args);

	System.out.println("I started at X = " + xPos + " Y = " + yPos);

	// Have the bug randomly walk backward (-1), forward (+1), or
	// not at all (0) in first the X and then the Y direction.
	// (The randomMove() method, defined below, returns an
	// integer between -1 and +1.)  Note that the bug's world is a
	// torus.  If the bug walks off the edge of its rectangular
	// world, it is magically transported (via the modulus
	// operator) to the opposite edge.
	for(i = 0; i < 100; i++)
	    {
	    xPos += randomMove();
	    yPos += randomMove();
	    xPos = (xPos + worldXSize) % worldXSize;
	    yPos = (yPos + worldYSize) % worldYSize;

	    System.out.println("I moved to X = " + xPos + " Y = " + yPos);
	    }

	return;
    }

    // Returns -1, 0 or +1 with equal probability.
    static int randomMove()
    {
	double randnum;

	// Math.random returns a pseudo random number in the interval
	// [0, 1).
	randnum = Math.random();

	if (randnum <= 0.33333)
	    return -1;
	else if (randnum <= 0.66667)
	    return 0;
	else
	    return 1;
    }
}
