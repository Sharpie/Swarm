import swarm.*;
import swarm.activity.*;
import swarm.objectbase.*;
import swarm.simtoolsgui.*;
import swarm.random.*;
import swarm.defobj.*;
import swarm.gui.*;
import swarm.analysis.*;
import swarm.space.*;
import swarm.random.*;

public class MousetrapStatistics
{
    public SwarmEnvironment se;
    public int numTriggered = 0;
    public int numBalls = 0;
    public void nag (String s)
    {
        System.out.println (this.getClass().getName() + ":" + s);
        System.out.flush ();
    }
    public Object addOneTriggered()
    {
        numTriggered ++;
        return this;
    }
    public Object addOneBall()
    {
        numBalls++;
        return this;
    }
    public Object removeOneBall()
    {
        if (numBalls > 0)
            numBalls--;
        else
            System.out.println ("Error: negative balls!\n");
        return this;
    }
    public int getNumTriggered()
    {
        return numTriggered;
    }
    public int getNumBalls()
    {
        return numBalls;
    }
}
