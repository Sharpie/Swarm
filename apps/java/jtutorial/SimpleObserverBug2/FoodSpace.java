// FoodSpace.java
// Defines the FoodSpace class as a subclass of Discrete2dImpl.

import swarm.Globals;
import swarm.defobj.Zone;
import swarm.space.Discrete2dImpl;

public class FoodSpace extends Discrete2dImpl
{
    // The constructor for a foodspace.  Create a foodspace in zone
    // aZone with dimensions xSize and ySize.  Note that a
    // food space is based on the Swarm's Discrete2dImpl class.
    public FoodSpace(Zone aZone, int xSize, int ySize)
    {
	// Call the constructor for the parent class and then fill the
	// lattice with zeros.
	super(aZone, xSize, ySize);
	fastFillWithValue(0);
    }

    // Method to randomly seed the foodspace with food.
    public void seedFoodWithProb(double seedProb)
    {
	int x, y;

	// Visit each cell and seed it with food with probability
	// seedProb. Note that the getDoubleWithMin$WithMax method has
	// been provided by the call to InitSwarm in
	// StartSimpleBug.java. Note too that we depend upon the getXSize()
	// and getYSize() methods inherited from the Discrete2dImpl
	// class to retrieve the dimensions of the foodspace.
	for (y = 0; y < getSizeY(); y++)
	    for (x = 0; x < getSizeX(); x++)
	       	if (Globals.env.uniformDblRand.getDoubleWithMin$withMax(0.0, 1.0) 
		        <= seedProb )
		    putValue$atX$Y(1, x, y);
    }

}
