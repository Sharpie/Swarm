/**
 * Wrapper object for an (x,y) co-ordinate */
public class HeatCell
{
    int x, y;
    
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
