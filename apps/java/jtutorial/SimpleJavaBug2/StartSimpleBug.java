// StartSimpleBug.java
// The Java SimpleBug application. 

import swarm.Globals;
import swarm.defobj.Zone;

public class StartSimpleBug
{
    // The size of a bug's world.
    static int worldXSize = 80;
    static int worldYSize = 80;

    // The probability for seeding the foodspace.
    static double seedProb = 0.5;

    public static void main (String[] args)
    {
	int i, xPos, yPos;
	SimpleBug abug;
	FoodSpace foodSpace;

	// Swarm initialization: all Swarm apps must call this first.
	Globals.env.initSwarm ("SimpleBug", "2.1", 
			       "bug-swarm@santafe.edu", args);

	// Create a foodspace of the desired dimension.
	foodSpace = new FoodSpace(Globals.env.globalZone, 
				   worldXSize, worldYSize);

	// Seed the foodspace with food.
	foodSpace.seedFoodWithProb(seedProb);

	// To place our new SimpleBug in the middle of its foodspace,
	// use foodSpace's own knowledge of its dimensions to find the
	// middle.
	xPos = (foodSpace.getSizeX())/2;
	yPos = (foodSpace.getSizeY())/2;

	// Create an instance of a SimpleBug, abug, and place it
	// within its foodSpace world at (xPos, yPos). The bug is
	// created in Swarm's globalZone and is given a "bug id" of
	// 1. Since foodSpace knows its dimensions, abug can get them
	// directly from the foodSpace.  We no longer have to provide
	// them in creating a SimpleBug.
	abug = new SimpleBug(Globals.env.globalZone, foodSpace, 
			     xPos, yPos, 1);

	// Loop our bug through a series of random walks in its
	// foodspace, asking it to report its position after each one.
	// The bug itself will report when it finds food.
	for (i = 0; i < 100; i++)
	{
	    abug.randomWalk();
	    abug.reportPosition();
	}
    }
}
