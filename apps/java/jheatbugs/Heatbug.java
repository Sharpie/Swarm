// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.Globals;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;
import swarm.gui.Raster;

/**
 * Heatbugs are agents in a 2d world with simple behaviour: if too
 * cold, move to warmer spot if too warm, move to cooler spot.  and
 * some occasional exceptions if the spot is occupied, try to move to
 * an unoccupied spot.  randomMoveProbability chance of moving to a
 * random spot */
public class Heatbug {
  /** my current unhappiness */
  public double unhappiness;			  
  /**  my spatial coordinates */
  public int x, y;			
  /**  my ideal temperature */
  public int idealTemperature;	
  /** how much heat I put out */
  public int outputHeat;				  
  /** chance of moving randomly */
  public double randomMoveProbability;			  
  /** the world I live in */
  Grid2d world;				  
  /** how big that world is */
  public int worldXSize, worldYSize;			  
  /** the heat for the world */
  HeatSpace heat;	
  /** my colour (display) */
  public byte bugColor;		

  /* Scratch cell for extracting return values */
  HeatCell scratchHeatCell;

  /** 
   * these methods are used to initialize the object's state. First,
   * methods that have to be sent to create an object. */
  
  /** 
   * Constructor for Heatbug 
   */
  public Heatbug (Grid2d w, HeatSpace h) {
    // Strictly speaking, this check isn't necessary. But we intend these
    // parameters to be immutable once set, so to be extrasafe we check:
    // it could catch an error later.

    if (world != null || heat != null)
      System.err.println ("You can only set the world/heat " +
                          "of a heatbug at creation time");
    
    world = w;
    heat = h;
    
    // make sure the user set up world and heat.
        
    if (world == null || heat == null)
      System.err.println ("Heatbug was created without a world or heat");
        
    // Cache the worldSize for speed of later access. Note how we
    // do this in createEnd - it could also have been done when
    // setWorld:Heat: was called, but this is a good place to do
    // it, too. If an object needed to allocate extra memory, this
    // is the right place to do it.
    
    worldXSize = world.getSizeX ();
    worldYSize = world.getSizeY ();

    // Someday, it'd be good if the space library to be powerful
    // enough that the heatbugs never need to be aware how big
    // their world is.

    scratchHeatCell = new HeatCell (0, 0);
  }
  
  /** 
      Methods for reading/writing a Heatbug's state during runtime.
      The probe mechanism is the lowlevel way of getting at an
      object's state - you're also allowed (but not required) to
      write methods to access the state as you find it is necessary
      or convenient.  Note the naming convention: for a variable
      named "fooBar" methods are -(sometype) getFooBar; -setFooBar;
      this naming convention will be important for a later version
      of probe.  (probe will preferentially use these methods
      instead of direct access).  */
  public double getUnhappiness () {
    return unhappiness;
  }
  
  /**
     Simple set methods for Heatbug state. Some of these are
     probably not going to normally change in a heatbugs lifetime,
     but there's no reason they couldn't change.  */
  public Object setIdealTemperature (int i) {
    idealTemperature = i;
    return this;
  }

  public Object setOutputHeat (int o) {
    outputHeat = o;
    return this;
  }
    
  public Object setRandomMoveProbability (double p) {
    randomMoveProbability =  p;
    return this;
  }

  /**
     This method is a bit dangerous: we blindly put ourselves on top
     of the grid no matter what's underneath us: because Grid2d only
     allows one object per square, we could be destroying data. This
     is poor design, but fortunately doesn't kill us in this
     particular app. If some other object really needed to find all
     objects based on looking in the grid, it would cause
     problems. (But note, in heatbug creation, how we tell Grid2d to
     turn off its warnings about overwrites) */
  public Object setX$Y (int inX, int inY) {
    x = inX;
    y = inY;
    world.putObject$atX$Y (this, x, y);		  // yikes!
    return this;
  }


  /**
     All of the previous code is basic Swarm object programming. The
     real simulation code follows.  Heatbug behaviour is actually
     implemented here. The notion of a "step" method is a nice
     simplification for basic simulations.
  */
  public void heatbugStep () {
    int heatHere;
    int newX, newY;
    int tries;

    // find out the heat where we are sitting.
        
    heatHere = heat.getValueAtX$Y (x, y);
    
    // update my current unhappiness value: abs(ideal - here);
        
    if (heatHere < idealTemperature)
      unhappiness = (double) (idealTemperature - heatHere) / HeatSpace.maxHeat;
    else
      unhappiness = (double) (heatHere - idealTemperature) / HeatSpace.maxHeat;
        
    // now ask the heatspace to tell us where the warmest or
    // coldest spot is The method call returns values back into
    // newX and newY.
        
    scratchHeatCell.x = x;
    scratchHeatCell.y = y;
      
    heat.findExtremeType$X$Y (((heatHere < idealTemperature)
                               ? HeatSpace.hot
                               : HeatSpace.cold),
                              scratchHeatCell);
    newX = scratchHeatCell.x;
    newY = scratchHeatCell.y;
        
    // After choice of ideal spot is made, there's a chance of
    // random move.  (Note the normalization of coordinates to [0,
    // worldSize). The current space library does not enforce
    // boundary conditions.)
    if ((Globals.env.uniformDblRand.getDoubleWithMin$withMax (0.0, 1.0)) 
        < randomMoveProbability)
      {
        // pick a random spot
        newX = 
          x + Globals.env.uniformIntRand.getIntegerWithMin$withMax (-1, 1); 
        
        newY = 
          y + Globals.env.uniformIntRand.getIntegerWithMin$withMax (-1, 1);
        
        // normalize coords
        newX = (newX + worldXSize) % worldXSize;      
        newY = (newY + worldYSize) % worldYSize;
      }

    // Part of the heatbug simulation is that two bugs cannot be
    // in the same spot. The code to enforce that is done here: if
    // the site we want is occupied by another heatbug, move
    // randomly. Note that this code does not parallelize
    // properly, it requires that each bug be set a "step" method
    // in sequence. This is a design flaw in heatbugs: proper
    // conflict resolution is difficult.  Also note we only look
    // for 10 random spots - if we don't find an unoccupied spot
    // by then, assume it's too crowded and just don't move.

    if (unhappiness == 0)
      { 
        // only update heat - don't move at all if no unhappiness
        heat.addHeat$X$Y (outputHeat, x, y);
      }
    else {
      tries = 0;

      // only search if the current cell is neither the optimum
      // or randomly chosen location - else don't bother
      if ( (newX != x || newY != y) )
        {
          while ( (world.getObjectAtX$Y (newX, newY) != null) 
                  && (tries < 10) )
            {
              int location, xm1, xp1, ym1, yp1;
              // choose randomly from the nine possible
              // random locations to move to
              location = 
                Globals.env.uniformIntRand.getIntegerWithMin$withMax (1,8);

              xm1 = (x + worldXSize - 1) % worldXSize;
              xp1 = (x + 1) % worldXSize;
              ym1 = (y + worldYSize - 1) % worldYSize;
              yp1 = (y + 1) % worldYSize;

              switch (location)
                {
                case 1:  
                  newX = xm1; newY = ym1;   // NW
                  break;  
                case 2:
                  newX = x ; newY = ym1;    // N
                  break;  
                case 3:
                  newX = xp1 ; newY = ym1;  // NE
                  break;  
                case 4:
                  newX = xm1 ; newY = y;    // W
                  break;  
                case 5:
                  newX = xp1 ; newY = y;    // E
                  break;  
                case 6:
                  newX = xm1 ; newY = yp1;  // SW
                  break;  
                case 7:
                  newX = x ; newY = yp1;    // S
                  break;  
                case 8:
                  newX = xp1 ; newY = yp1;  // SE
                default:
                  break;
                }
          
              tries++;			// don't try too hard.
            }
          if (tries == 10)
            {
              // no nearby clear spot, so just don't move.
              newX = x;
              newY = y;
            }
        }
      // Phew - we've finally found a spot to move ourselves, in
      // (newX, newY). Update heat where we were sitting.
  
      heat.addHeat$X$Y (outputHeat, x, y);
      
      // Now move ourselves in the grid and update our coordinates.
            
      world.putObject$atX$Y (null, x, y);
      x = newX;
      y = newY;
      world.putObject$atX$Y (this, newX, newY);
    }
        
    // all done moving! Return this.
  }

  /**
     Extra bits of display code: setting our colour, drawing on a
     window.  This code works, but it'd be better if there were a
     generic object that knew how to draw agents on grids.  */
  public Object setBugColor(byte c) { 
    bugColor = c; 
    return this; 
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, bugColor);
    return this;
  }
}
