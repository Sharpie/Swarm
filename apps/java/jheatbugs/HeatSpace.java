// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// Bits to support a specialization of diffusion objects: "heat space".
// Most of the real work is done in Diffuse, which implements a CA.
// These functions simplify and stereotype access to the space variable,
// making the Heatbug code higher level.

import swarm.Globals;

import swarm.defobj.Zone;

import swarm.space.Diffuse2dImpl;

import java.util.List;
import java.util.ArrayList;

/** The HeatSpace is a simple object to represent the heat in the
 * world: a spatial variable. HeatSpace inherits most of its
 * behaviour from the "Diffuse2dImpl" space Object. Inherit from
 * Diffuse2dImpl, don't add any new variables */
public class HeatSpace extends Diffuse2dImpl {
  
    /** constant: maximum heat.  This could just be used from the
        Diffuse2d object's max states. */
    public static final int maxHeat = 0x7fff;
  
    // used in findExtremeTypeX$Y
    static final int cold = 0, hot = 1;

    public int sizeX, sizeY;

    public HeatSpace (Zone aZone, int worldXSize, int worldYSize, 
                      double diffuseConstant, double evaporationRate)
    {
        super (aZone, worldXSize, worldYSize, diffuseConstant, 
               evaporationRate);
	sizeX = worldXSize;
	sizeY = worldYSize;
    }
    
    /** 
     * Add heat to the current spot. This code checks the bounds on
     maxHeat, pegs value at the top. */
    public Object addHeat$X$Y (int moreHeat, int x, int y) {
        int heatHere;
        
        heatHere = getValueAtX$Y (x, y);	  // read the heat
        if (moreHeat <= maxHeat - heatHere)   // would add be too big?
            heatHere = heatHere + moreHeat;	  // no, just add
        else
            heatHere = maxHeat;		  // yes, use max
        putValue$atX$Y (heatHere, x, y);	  // set the heat
        return this;
    }
    
    /**
     *  Search the 9 cell neighbourhood for the requested extreme
     * (cold or hot) The X and Y arguments are used both as input
     * (where to search from) and as output (pointers are filled with
     * the coordinate of the extreme).  Note that wraparound edges
     * (boundary conditions) are implicitly in the code - look at the
     * call to getValueAtX$Y. */

    public int findExtremeType$X$Y (int type, HeatCell hc) {
      int bestHeat;
      int x, y;
      List heatList;
      HeatCell cell, bestCell;
      int offset;
      int px = hc.x;
      int py = hc.y;
      
        // prime loop: assume extreme is right where we're standing
      bestHeat = getValueAtX$Y (px, py);
      
      // Now scan through the world, finding the best cell in the 8
        // cell nbd.  To remove the bias from the choice of location,
      // we keep a list of all best ones and then choose a random
      // location if there are points of equal best heat.
      heatList = new ArrayList ();
      
      for (y = py - 1; y <= py + 1; y++) {  
	for (x = px - 1; x <= px + 1; x++) {
	  int heatHere;
	  boolean hereIsBetter, hereIsEqual;
	  
	  heatHere = getValueAtX$Y ((x + sizeX) % sizeX,
				    (y + sizeY) % sizeY);
	  
	  hereIsBetter = (type == cold) ? (heatHere < bestHeat)
	    : (heatHere > bestHeat);
	  
	  hereIsEqual = (heatHere == bestHeat);
	  
	  if (hereIsBetter) {	      // this spot more extreme
	    
	    cell = new HeatCell (x, y);
	    
	    // this heat must be the best so far, so delete all the
	    // other cells we have accumulated
	    heatList.clear ();
	    heatList.add (cell); 
	    
	    // now list only has the one new cell
	    
	    bestHeat = heatHere;   // update information
	  }
          
	  // if we have spots of equal best heat - then we add to the
	  // list from which we can choose randomly later
	  if (hereIsEqual) {
	    cell = new HeatCell (x, y);
	    heatList.add (cell); // add to the end of the list
	  }
	}
	
      }
      // choose a random position from the list
      offset = Globals.env.uniformIntRand.getIntegerWithMin$withMax 
	(0, (heatList.size () - 1));
      
      // choose a point at random from the heat list
      bestCell = (HeatCell) heatList.get (offset);
      
      // Now we've found the requested extreme. Arrange to return the
      // information (normalize coordinates), and return the heat we found.
      
      hc.setX ((bestCell.x + sizeX) % sizeX);
      hc.setY ((bestCell.y + sizeY) % sizeY);
      
      // clean up the temporary list of (x,y) points
      heatList.clear ();
      
      return getValueAtX$Y (hc.x, hc.y);
    }
}
