// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// All added comments copyright 2001 Timothy Howe. All rights reserved. 

import diagnostic.*;
import java.awt.*;

import swarm.Globals;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;
import swarm.gui.Raster;

/**
See HeatbugModelSwarm for an overview of the heatbugs application.

<p>
A Heatbug is an agent in a 2-dimensional world. A Heatbug has the following
behavior:

 <dir>
 A Heatbug has an ideal temperature (which is a property of the individual
 Heatbug), and a color that indicates the Heatbug's ideal 
 temperature (more green for cool-loving Heatbugs, more yellow for 
 warmth-loving Heatbugs).
 </dir>

 <dir>
 A Heatbug can sense the temperature of the cells in its 9-cell neighborhood.
 </dir>

 <dir>
 A Heatbug has an unhappiness, which is equal to the difference between the
 Heatbug's ideal temperature and the temperature of the cell where it sits.
 </dir>

 <dir>
 With an arbitrary probability (which is a property of the individual
 Heatbug), a Heatbug will move to a randomly-chosen empty cell in its 9-cell
 neighborhood.
 </dir>

 <dir>
 If a Heatbug does not move in the arbitrary fashion described in
 the previous paragraph, it will move to an empty cell in
 its 9-cell neighborhood whose temperature is closest to
 its ideal temperature. If there is more than once such
 cell, it will choose at random among the cells with that closest-to-ideal
 temperature.
 </dir>

 <dir>
 If a Heatbug does not move in the rational fashion described in
 the previous paragraph, it will make 10 attempts to move to a 
 randomly-chosen empty cell in its immediate neighborhood. 
 <dir>

 </dir>
 In all cases a Heatbug move only if its unhappiness is non-zero.
 </dir>

 <dir>
 Two or more Heatbugs may not occupy a given cell simultaneously. However,
 for simpler code, we do allow some collisions at initialization time -- the 
 Heatbugs quickly separate themselves.
 </dir>

 <dir>
 A Heatbug produces heat (the amount is a property of the individual Heatbug),
 but it deposits the heat at the cell where it was sitting, not at the cell
 it is going to.
 </dir>

*/
public class Heatbug
{
// The amount of heat I produce (units are undefined):
private int _outputHeat;
    public Object setOutputHeat (int outputHeat)
    { _outputHeat = outputHeat; return this; }
// The temperature I prefer:
private int _idealTemperature;
    public int getIdealTemperature () { return _idealTemperature; }
    public Object setIdealTemperature (int idealTemperature)
    { _idealTemperature = idealTemperature; return this; }
// The difference between my temperature and my ideal temperature:
private double _unhappiness;
    public double getUnhappiness () { return _unhappiness; }
// The chance that I will move arbitrarily:
private double _randomMoveProbability;
    public Object setRandomMoveProbability (double randomMoveProbability)
    { _randomMoveProbability = randomMoveProbability; return this; }
// The 2-dimensional world of motion:
private Grid2d _world;
// The 2-dimensional world of heat:
private HeatSpace _heatSpace;
// My location in _world as well as in _heatSpace:
private int _x, _y;
// My index into the ColorMap defined in HeatbugModelSwarm:
private byte _colorIndex;
    public void setColorIndex (byte colorIndex)
    { _colorIndex = colorIndex; }
// The model I belong to:
private HeatbugModelSwarm _model;
// My index in the Heatbug list (needed only for diagnostics):
private int _heatbugIndex;
// The level of diagnostics to write to standard output:
private int _printDiagnostics = 0;
    public void setPrintDiagnostics (int printDiagostics)
    { _printDiagnostics = printDiagostics; }

public Heatbug 
 (Grid2d world, 
  HeatSpace heatSpace, 
  HeatbugModelSwarm model,
  int heatbugIndex,
  int printDiagnostics
 )
{
    _world = world;
    _heatSpace = heatSpace;
    _model = model;
    _heatbugIndex = heatbugIndex;
    _printDiagnostics = printDiagnostics;

    if (_world == null)
        System.err.println ("Heatbug was created without a world");

    if (_heatSpace == null)
        System.err.println ("Heatbug was created without a heatSpace");

} /// constructor

public Object drawSelfOn (Raster raster)
{
    raster.drawPointX$Y$Color (_x, _y, _colorIndex);
    return this;
}

/**
This method defines what the Heatbug does whenever the Schedule triggers it. 

<p>
The method is synchronized, which means the compiler will not let it be 
multi-threaded, which means it cannot be parallelized. It is synchronized 
because to avoid collisions, the Heatbugs must decide one at a time which 
cell to move to. 

<p>
There may be other methods in this simulation that should be synchronized. 

*/
public synchronized void heatbugStep ()
{
    int newX, newY;

    // Get the heat where I am sitting:
    int heatHere = _heatSpace.getValueAtX$Y (_x, _y);

    // Update my current unhappiness:
    _unhappiness
     = (double) Math.abs (_idealTemperature - heatHere) 
     / (_model.getActivity ().getScheduleActivity ().getCurrentTime () + 1);
     /* ... The divisor is an attempt to neutralize the effect of the 
        increasing heat of the HeatSpace. (We add 1 just to avoid a 
        divide-by-zero error at step 0.) Without the divisor, Heatbugs would 
        keep getting happier as the heat increases, even if they're immobile
        or they move only randomly. Our real interest is in the happiness of 
        Heatbugs that is due to their motion. 

        According to the documentation for Diffuse2d, newHeat = "evapRate * 
        (self + diffusionConstant*(nbdavg - self)) where nbdavg is the weighted 
        average of the 8 neighbours". Ignoring the word "weighted", we think
        the formula implies that total heat will always be some constant times
        the number of steps, less heat discarded by _heatSpace.addHeat(). The 
        evaporation rate would affect the constant. Diffusion should cancel 
        itself out. 

        We're missing something here, because when we invoke current -i -d0 -e1
        -p20, total heat divided by step count drops continually, even when
        heat is not being discarded. And yet the Heatbugs keep getting happier, 
        even with step count in the divisor of the calculation of unhappiness. 

        Todo: figure out why total heat / step count keeps sinking. Figure out
        why unhappiness keeps sinking. Try an ideal heat well below MAX_HEAT
        and see if unhappiness eventually increases. 
    */

    if (_unhappiness != 0 && ! _model.getImmobile ())
    {

        double uDR = Globals.env.uniformDblRand.getDoubleWithMin$withMax (0.0, 1.0);
        if (uDR < _randomMoveProbability)
        {
            if (_printDiagnostics >= 100)
                System.out.print ("Moving randomly ... ");
            // Pick a random cell within the 9-cell neighborhood, applying
            // geographic wrap-around:
            newX =
             (_x + Globals.env.uniformIntRand.getIntegerWithMin$withMax (-1, 1)
              + _world.getSizeX ()
             ) % _world.getSizeX ();
            newY =
             (_y + Globals.env.uniformIntRand.getIntegerWithMin$withMax (-1, 1)
              + _world.getSizeY ()
             ) % _world.getSizeY ();
        } else
        {
            if (_printDiagnostics >= 100)
                System.out.print ("Moving rationally ... ");
            Point scratchPoint = new Point (_x, _y);
            // Ask the HeatSpace for a cell in the 9-cell neighborhood
            // with the closest-to-ideal temperature: 
            _heatSpace.findExtremeType$X$Y
             ((heatHere < _idealTemperature ? HeatSpace.HOT : HeatSpace.COLD),
              scratchPoint,   // scratchPoint is an inout parameter
              _world
             );
            newX = scratchPoint.x;
            newY = scratchPoint.y;
        }
        // ... Whether it chose randomly or rationally, a Heatbug may have
        // chosen the cell it is already at. If it did, the choice is about
        // to be rejected, since the code below checks to see whether the cell
        // is already occupied, without asking which Heatbug is occupying it:
        if (_world.getObjectAtX$Y (newX, newY) != null)
        {
            int tries = 0;
            int location, xm1, xp1, ym1, yp1;
            // 10 is an arbitrary choice for the number of tries; it is
            // *not* implied by the number of cells in the neighborhood:
            while ((_world.getObjectAtX$Y (newX, newY) != null) && 
                   (tries < 10)
                  )
            {
                // Choose randomly among the 8 cells in the neighborhood
                location = Globals.env.uniformIntRand.getIntegerWithMin$withMax (1,8);
                xm1 = (_x + _world.getSizeX () - 1) % _world.getSizeX ();
                xp1 = (_x + 1) % _world.getSizeX ();
                ym1 = (_y + _world.getSizeY () - 1) % _world.getSizeY ();
                yp1 = (_y + 1) % _world.getSizeY ();
                switch (location)
                {
                case 1:  
                    newX = xm1; newY = ym1;   // NW
                break;  
                case 2:
                    newX = _x ; newY = ym1;    // N
                break;  
                case 3:
                    newX = xp1 ; newY = ym1;  // NE
                break;  
                case 4:
                    newX = xm1 ; newY = _y;    // W
                break;  
                case 5:
                    newX = xp1 ; newY = _y;    // E
                break;  
                case 6:
                    newX = xm1 ; newY = yp1;  // SW
                break;  
                case 7:
                    newX = _x ; newY = yp1;    // S
                break;  
                case 8:
                    newX = xp1 ; newY = yp1;  // SE
                default:
                break;
                }
                tries++;
            }
            if (tries == 10)
            {
                if (_printDiagnostics >= 100)
                    System.out.println ("no, staying put ... ");
                newX = _x;
                newY = _y;
            }
            else
            {
                if (_printDiagnostics >= 100)
                    System.out.println ("no, desperately ... ");
            }
        }

        // Deposit heat at my old location; move to my new location. We
        // never subtract heat -- so even if the Heatbugs don't move, they
        // may still become happier:
        _heatSpace.addHeat (_outputHeat, _x, _y);
        _world.putObject$atX$Y (null, _x, _y);
        _x = newX;
        _y = newY;
        _world.putObject$atX$Y (this, _x, _y);

    } /// if _unhappiness != 0
    else
    {
        if (_printDiagnostics >= 100)
        {
            System.out.println ("Too happy to move ... ");
        }
        _heatSpace.addHeat (_outputHeat, _x, _y);
    }

    if (_printDiagnostics >= 100)
        System.out.println ("Heatbug " + this);

} /// heatbugStep()

/**
This method does not check to see whether the target cell is already occupied.
*/
public Object putAtX$Y (int inX, int inY)
{
    _x = inX;
    _y = inY;
    _world.putObject$atX$Y (this, _x, _y);
    return this;
}

/**
The Java compiler will invoke this method whenever we use a Heatbug where the
compiler is expecting a String. That gives us an easy way to print diagnostics;
for example, System.out.println ("I initialized Heatbug " + heatbug + ".");.
*/
public String toString ()
{
    return _heatbugIndex + " at (" + _x + "," + _y + "), heat " + _heatSpace.getValueAtX$Y (_x, _y);
}

} /// class Heatbug
