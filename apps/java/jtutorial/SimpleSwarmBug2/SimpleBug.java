// SimpleBug.java
// Defines the class for our SimpleBug agents/

import swarm.Globals;
import swarm.defobj.Zone;
import swarm.objectbase.SwarmObjectImpl;
import swarm.space.Grid2dImpl;

public class SimpleBug extends SwarmObjectImpl
{
    // These instance variables keep track of a given bug's foodspace,
    // position and identity.  We also save the dimensions of the
    // foodspace so that we can make fewer calls to the getSizeX() and
    // getSizeY() methods in the bug's randomWalk().
    public FoodSpace myFoodSpace;
    public Grid2dImpl myBugSpace;
    public int xPos;
    public int yPos;
    public int bugNumber;

    int worldXSize;
    int worldYSize;

    // haveEaten keeps track of whether the bug has eaten on its most
    // recent walk.
    public boolean haveEaten;

    // Constructor to create a SimpleBug object in Zone aZone and to
    // place it in the foodspace and bugspace, fSpace and bSpace, at
    // the specified X,Y location. The bug is also given a numeric id,
    // bNum.
    public SimpleBug(Zone aZone, FoodSpace fSpace, Grid2dImpl bSpace, 
		     int X, int Y, int bNum)
    {
	// Call the constructor for the bug's parent class.
	super(aZone);

	// Record the bug's foodspace, bugspace, initial position and
	// id number.
	myFoodSpace = fSpace;
	myBugSpace = bSpace;
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
    // food, it eats it and turns on the haveEaten flag so it can
    // report its feast if asked.  Note that before the bug actually
    // moves, we must check to see that there is no other bug at the
    // destination cell.  If there is, the this bug just stays put.
    public void randomWalk()
    {
	int newX, newY;

	// Turn off the haveEaten flag.
	haveEaten = false;

	// Decide where to move.
	newX = xPos + 
            Globals.env.uniformIntRand.getIntegerWithMin$withMax(-1, 1);
	newY = yPos +
	    Globals.env.uniformIntRand.getIntegerWithMin$withMax(-1, 1);
	newX = (newX + worldXSize) % worldXSize;
	newY = (newY + worldYSize) % worldYSize;

	// Is there a bug at the new position already? If not, put a
	// null at this bug's current position and put this bug at the
	// new position.
	if (myBugSpace.getObjectAtX$Y(newX, newY) == null)
	    {
		myBugSpace.putObject$atX$Y(null, xPos, yPos);
		xPos = newX;
		yPos = newY;
		myBugSpace.putObject$atX$Y(this, xPos, yPos);
	    }

	// If there is food at this cell, eat it and set the haveEaten
	// flag.
	if (myFoodSpace.getValueAtX$Y(xPos, yPos) == 1)
	    {
	    myFoodSpace.putValue$atX$Y(0, xPos, yPos);
	    haveEaten = true;
	    }
    }

    // Method to report the bug's position to the console.
    public void reportPosition()
    {
	System.out.println("Bug " + bugNumber + " is at " + xPos + 
			   ", " + yPos);
    }
    // Method to report if the bug has eaten.
    public boolean reportIfEaten()
    {
	if ( haveEaten )
	    System.out.println("Bug " + bugNumber + " has found food at " +
			       xPos + ", " + yPos);

	return haveEaten;
    }
}

