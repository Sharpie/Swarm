// SimpleBug.java
// Defines the class for our SimpleBug agents/

import swarm.Globals;
import swarm.defobj.Zone;
import swarm.objectbase.SwarmObjectImpl;

public class SimpleBug extends SwarmObjectImpl
{
    // These instance variables keep track of a given bug's foodspace,
    // position and identity.  We also save the dimensions of the
    // foodspace so that we can make fewer calls to the getSizeX() and
    // getSizeY() methods in the bug's randomWalk().
    FoodSpace myFoodSpace;
    public int xPos;
    public int yPos;
    public int bugNumber;

    int worldXSize;
    int worldYSize;

    // Constructor to create a SimpleBug object in Zone aZone and to
    // place it in the foodspace, fSpace, at the specified X,Y
    // location. The bug is also given a numeric id, bNum.
    public SimpleBug(Zone aZone, FoodSpace fSpace, int X, int Y, 
		     int bNum)
    {
	// Call the constructor for the bug's parent class.
	super(aZone);

	// Record the bug's foodspace, initial position and id number.
	myFoodSpace = fSpace;
	worldXSize = myFoodSpace.getSizeX();
	worldYSize = myFoodSpace.getSizeY();
	xPos = X;
	yPos = Y;
	bugNumber = bNum;
	
	// Announce the bug's presence to the console.
	System.out.println("SimpleBug number " + bugNumber + 
		     " has been created at " + xPos + ", " + yPos);
    }

    // This is the method to have the bug take a random walk backward
    // (-1), forward (+1), or not at all (0) in first the X and then
    // the Y direction.  The randomWalk method uses
    // getIntegerWithMin$withMax() to return an integer between a
    // minimum and maximum value, here between -1 and +1.
    // Globals.env.uniformRand is an instance of the class
    // UniformIntegerDistImpl, instantiated by the call to
    // Globals.env.initSwarm in StartSimpleBug.  Note that the bug's
    // world is a torus.  If the bug walks off the edge of its
    // rectangular world, it is magically transported (via the modulus
    // operator) to the opposite edge.  If on its walk the bug finds
    // food, it eats it.
    public void randomWalk()
    {
	xPos += Globals.env.uniformIntRand.getIntegerWithMin$withMax(
								-1, 1);
	yPos += Globals.env.uniformIntRand.getIntegerWithMin$withMax(
								-1, 1);
	xPos = (xPos + worldXSize) % worldXSize;
	yPos = (yPos + worldYSize) % worldYSize;

	// If there is food at this cell, eat it!
	if (myFoodSpace.getValueAtX$Y(xPos, yPos) == 1)
	    {
	    myFoodSpace.putValue$atX$Y(0, xPos, yPos);
	    System.out.println("Bug " + bugNumber + " has found food at " + xPos 
			      + ", " + yPos);
	    }
    }

    // Method to report the bug's position to the console.
    public void reportPosition()
    {
	System.out.println("Bug " + bugNumber + " is at " + xPos + 
			   ", " + yPos);
    }
}
