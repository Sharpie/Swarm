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

public class Mousetrap 
{
    public int xCoord;
    public int yCoord;
    public boolean triggered;
    public ZoomRasterImpl displayWidget;
    public MousetrapModelSwarmImpl modelSwarm;
    public UniformDoubleDistImpl uniform0to1;
    public UniformIntegerDistImpl uniformRadius;
    public UniformUnsignedDistImpl uniformTrigTime;
    public SwarmEnvironment se;

    public void nag (String s)
    {
        System.out.println (this.getClass().getName() + ":" + s);
        System.out.flush ();
    }

    public Mousetrap (MousetrapModelSwarmImpl s, int x, 
                      int y, Object randGenerator)
    {
        int maxD;
        UniformDoubleDistCImpl iudbld;
        UniformIntegerDistCImpl iuintd;
        UniformUnsignedDistCImpl iuunsd;
    
        modelSwarm = s;
        xCoord = x;
        yCoord = y;
    
        maxD = modelSwarm.getMaxTriggerDistance();
        uniform0to1 = new  UniformDoubleDistImpl ();
        iudbld = new UniformDoubleDistCImpl (uniform0to1);
    
        uniform0to1 = (UniformDoubleDistImpl) iudbld.create$setGenerator$setDoubleMin$setMax (modelSwarm.getZone(), randGenerator, 0.0, 1.0);

        uniformRadius = new  UniformIntegerDistImpl ();
        iuintd = new UniformIntegerDistCImpl (uniformRadius);
  
        uniformRadius =  (UniformIntegerDistImpl) iuintd.create$setGenerator$setIntegerMin$setMax (modelSwarm.getZone(), randGenerator, -maxD, maxD);

        uniformTrigTime =  new UniformUnsignedDistImpl (); 
        iuunsd = new UniformUnsignedDistCImpl (uniformTrigTime);

        uniformTrigTime = (UniformUnsignedDistImpl) iuunsd.create$setGenerator$setUnsignedMin$setMax (modelSwarm.getZone(), randGenerator, 1, modelSwarm.getMaxTriggerTime());
    }
    public Object setDisplayWidget (ZoomRasterImpl w)
    {
        displayWidget = w;
        return this;
    }
    public void noMethod ()
    {
    }
  
    public void trigger()
    {
        int n, xTrigger, yTrigger;
        int triggerTick;

        ((MousetrapStatistics)modelSwarm.getStats()).removeOneBall();

        if (!triggered
            && modelSwarm.getTriggerLikelihood() >= 1 
            || ((float)uniform0to1.getDoubleSample() 
                < modelSwarm.getTriggerLikelihood()))  {
            
            int size;
            triggered = true;
            modelSwarm.getStats().addOneTriggered();
            
            if (displayWidget != null)
                displayWidget.drawPointX$Y$Color (xCoord, yCoord, (byte) 2);
            size = modelSwarm.getGridSize();
            for (n = modelSwarm.getNumberOutputTriggers(); n>0; n--) {
                Mousetrap trap;
                xTrigger = 
                    (xCoord + size + uniformRadius.getIntegerSample()) % size;
                yTrigger = 
                    (yCoord + size + uniformRadius.getIntegerSample()) % size;
                
                triggerTick = se.getCurrentTime() + 
                    uniformTrigTime.getUnsignedSample();
                trap = modelSwarm.getMousetrapAtX$Y (xTrigger, yTrigger);
                
                if (trap != null) {
                    ((MousetrapModelSwarmImpl) modelSwarm).getStats().addOneBall();
                    modelSwarm.scheduleTriggerAt$For(triggerTick, trap);
                }
            }
        }
        //    return this;
    }
}


class StartMousetrap
{
    public static void nag (String s)
    {
        System.out.println ( s);
    }
    public static void main (String[] args)
    {
        SwarmEnvironment se = new SwarmEnvironment (args);
        MousetrapObserverSwarmImpl topLevelSwarm = 
            new MousetrapObserverSwarmImpl();
        MousetrapObserverSwarmCImpl cswarm = 
            new MousetrapObserverSwarmCImpl (topLevelSwarm);
   
        //    nag ("main");
        cswarm.se = se;
        //    nag ("createBegin");
        cswarm.createBegin (se.globalZone);
        //    nag ("setWindowGeometryRecordName");
        se.setWindowGeometryRecordName (cswarm);
        //    nag ("createEnd");
        topLevelSwarm = (MousetrapObserverSwarmImpl) cswarm.createEnd ();
        //    nag("done createEnd");
        topLevelSwarm.se = se;
        //    nag ("build Objects");
        topLevelSwarm.buildObjects ();
        //    nag ("build actions");
        topLevelSwarm.buildActions ();
        //    nag ("activate in");
        topLevelSwarm.activateIn (null);
        //    nag ("go");
        ((SwarmActivityImpl)topLevelSwarm.getActivity()).run();
    }
}






