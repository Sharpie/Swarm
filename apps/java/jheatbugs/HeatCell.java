// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

/**
 * Wrapper object for an (x,y) co-ordinate */
public class HeatCell
{
    public int x, y;
    
    public HeatCell (int theX, int theY)
    {
        x = theX;
        y = theY;
    }
    public Object setX (int theX)
    {
        x = theX;
        return this;
    }
    public Object setY (int theY)
    {
        y = theY;
        return this;
    }
    
    public int getX ()
    { 
        return x;
    }
    public int getY ()
    {
        return y;
    }
}
