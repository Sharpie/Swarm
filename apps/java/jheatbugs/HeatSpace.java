// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// All added comments copyright 2001 Timothy Howe. All rights reserved. 

import swarm.Globals;

import swarm.defobj.Zone;

import swarm.space.Diffuse2dImpl;
import swarm.space.Grid2d;

import java.awt.*;   // We use this just for class Point.
import java.util.ArrayList;

/**
See HeatbugModelSwarm for an overview of the heatbugs application.

<p>
Swarm's class Diffuse2dImpl provides a 2-dimensional array of integer values.
Behavior of Diffuse2dImpl includes both diffusion and "evaporation" of whatever
the values represent, at rates we can specify.

<p>
The class HeatSpace specializes Diffuse2dImpl; we use the integer values
to represent heat (which Heatbugs produce as well as seek).

<p>
We use <tt>Diffuse2dImpl.getValueAtX$Y()</tt> 
and <tt>Diffuse2dImpl.putValue$atX$Y()</tt> to
access the integer values.

<p>
You may wonder why the name of the method is <i>putValue$atX$Y()</i> rather 
than <i>setValue$atX$Y()</i>. The name reflects the fact that 
the change is buffered and 
therefore does not have the instant effect that you would expect from
a method named <i>setValue$atX$Y()</i>. In particular, if you change a value by
invoking <tt>putValue$atX$Y()</tt> and then immediately invoke
<tt>getValue$atX$Y()</tt>, you will still see the old heat value.

*/
public class HeatSpace extends Diffuse2dImpl
{

public static final int COLD = 0, HOT = 1;
public static final int MAX_HEAT = 0x7fff;

private int _printDiagnostics = 0;
    public void setPrintDiagnostics (int printDiagostics)
    { _printDiagnostics = printDiagostics; }

public HeatSpace
 (Zone aZone,
  int worldXSize,
  int worldYSize,
  double diffusionConstant,
  double evaporationRate,
  int printDiagnostics
 )
{
    super (aZone, worldXSize, worldYSize, diffusionConstant, evaporationRate);
    _printDiagnostics = printDiagnostics;
} /// constructor

/**
This method adds heat to the current cell, never exceeding MAX_HEAT.
*/
public Object addHeat (int moreHeat, int x, int y)
{
    int heatHere;

    heatHere = getValueAtX$Y (x, y);      // read the heat
    int oldHeatHere = heatHere;

    if (heatHere + moreHeat <= MAX_HEAT)   // would add be too big?
        heatHere = heatHere + moreHeat;      // no, just add
    else
    {
        heatHere = MAX_HEAT;                  // yes, use max
        if (_printDiagnostics >= 21)
            System.out.println
             ("In HeatSpace.addHeat() at (" + x + "," + y + "), I discarded heat " + heatHere + " + " + moreHeat + " - " + MAX_HEAT + " = " + (heatHere + moreHeat - MAX_HEAT) + ".");
    }

    putValue$atX$Y (heatHere, x, y);      // set the heat
    // Monitor the heat at an arbitrary cell (2, 2) (HeatbugModelSwarm monitors 
    // the same cell):
    if (_printDiagnostics >= 10 && x == 2 && y == 2)
    {
        System.out.println 
         ("In HeatSpace.addHeat(), heat " + moreHeat + " was added at (" + x + ", " + y + ") to change from " + oldHeatHere + " to " + heatHere + ".");
    }
    return this;
} /// addHeat()

/**
This method searches targetCell's 9-cell neighborhood for the requested 
extreme (COLD or HOT). 

<p>Note that a HeatSpace has a wrap-around geography: 
the south-eastern neighbor of the cell at (x, y) is the cell at ((x + 1) % 
getSizeX (), (y - 1) % getSizeY ()). Of course, <tt>getSizeX()</tt> 
and <tt>getSizeY()</tt> are inherited from Diffuse2dImpl.

@param type (in)
    HeatSpace.COLD or HeatSpace.HOT
@param targetCell (inout)
    the Point at the center of the 9-cell neighborhood; altered to a Point
    randomly selected from among those with the most-ideal temperature
*/

public int findExtremeType$X$Y (int type, Point targetCell, Grid2d world)
{
    int x, y;
    Point candidate;

    // Prime the loop by assuming that the extreme heat is right at targetCell:
    int bestHeat = getValueAtX$Y (targetCell.x, targetCell.y);

    // Scan through the 9-cell neighborhood, keeping a list of all cells
    // that tie for most extreme:
    ArrayList heatList = new ArrayList ();

    for (y = targetCell.y - 1; y <= targetCell.y + 1; y++)
    {
        for (x = targetCell.x - 1; x <= targetCell.x + 1; x++)
        {

            candidate = new Point (x % getSizeX (), y % getSizeY ());

            int candidateHeat = getValueAtX$Y (candidate.x, candidate.y);

            boolean candidateIsBetter = (type == COLD)
             ? (candidateHeat < bestHeat)
             : (candidateHeat > bestHeat);

            boolean candidateIsEqual = (candidateHeat == bestHeat);

            if (candidateIsBetter)
            {
            // ... This cell is more extreme than any so far.
                // Delete all the other cells we have accumulated:
                heatList.clear ();
                heatList.add (new Point (x, y));
                bestHeat = candidateHeat;
            }
            else if (candidateIsEqual)
            {
                heatList.add (new Point (x, y));
            }
        }
    }

    // Choose a point at random from the list of Points tied for most extreme:
    int offset = Globals.env.uniformIntRand.getIntegerWithMin$withMax
     (0, (heatList.size () - 1));
    Point bestCell = (Point) heatList.get (offset);

    // We've found the requested extreme. Set the (inout) variable targetCell,
    // applying geographic wrap-around, and return the heat we found:
    targetCell.x = ((bestCell.x + getSizeX ()) % getSizeX ());
    targetCell.y = ((bestCell.y + getSizeY ()) % getSizeY ());

    return bestHeat;

} /// findExtremeType$X$Y()

/**
Did you think you could override Diffuse2d.stepRule()? You can -- as we have
done here -- but the method will never be invoked.

<p>
We know the method is defined "correctly" because 
if we alter the signature, for example to return void rather than Object, 
the compiler complains that the method does not match the method
"inherited from type 'swarm/space/Diffuse2dImpl'". 

<p>
Diffuse2d inherits from Ca2d, which inherits from DblBuffer2d, which maintains
two lattices (old and new). As the Swarm Reference Guide states for 
DblBuffer2d, <tt>putValue()</tt> -- which <tt>stepRule()</tt> presumably
invokes -- is "overridden so writes happen to newLattice".

*/
public Object stepRule ()
{
    // super.stepRule ();
    System.out.println ("This method never gets invoked.");
    return this;
}

/**
This method returns the sum of the heat in each cell of the HeatSpace.

*/
public double totalHeat ()
{
    double totalHeat = 0.0;
    for (int x = 0; x < getSizeX (); x++)
        for (int y = 0; y < getSizeY (); y++)
            totalHeat += getValueAtX$Y (x, y);
    return totalHeat;
}

} /// class HeatSpace
