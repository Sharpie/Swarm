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

/**
 This class implements a `Mousetrap' agent. Mousetraps that live in
 fixed positions on a 2D lattice. Each mousetrap has N "ping-pong"
 balls. When "triggered" (hit by another ping-pong ball) it releases
 its ping-pong balls into the air, which hit other mousetraps and
 trigger them.  Tossing ping-pong balls into the air to trigger other
 mousetraps is accomplished by picking "nearby" mousetraps, and
 arranging for them to be sent trigger messages in the "near" future.  */
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

  public void nag (String s)
  {
    System.out.println (this.getClass().getName() + ":" + s);
    System.out.flush ();
  }

  public Mousetrap (MousetrapModelSwarmImpl s, int x, 
                    int y, Object randGenerator)
  {
    int maxD;
    
    modelSwarm = s;
    xCoord = x;
    yCoord = y;
    
    maxD = modelSwarm.getMaxTriggerDistance();
    uniform0to1 = new  UniformDoubleDistImpl 
      ((ZoneImpl)modelSwarm.getZone(), randGenerator, 0.0, 1.0);

    uniformRadius = new UniformIntegerDistImpl     
      ((ZoneImpl)modelSwarm.getZone(), randGenerator, -maxD, maxD);

    uniformTrigTime =  new UniformUnsignedDistImpl 
      ((ZoneImpl)modelSwarm.getZone(), randGenerator, 1, 
       modelSwarm.getMaxTriggerTime());
  }

  /**
   * Here we record who to display ourselves on.
   */
  public Object setDisplayWidget (ZoomRasterImpl w)
  {
    displayWidget = w;
    return this;
  }
  public void noMethod ()
  {
  }

  /**
   * The crucial step for a Mousetrap (equivalent to "step" for a Heatbug)  
   */
  public void trigger()
  {
    int n, xTrigger, yTrigger;
    int triggerTick;

    ((MousetrapStatistics)modelSwarm.getStats()).removeOneBall();

    if (!triggered
        && ((modelSwarm.getTriggerLikelihood() >= 1)
            || ((float)uniform0to1.getDoubleSample() 
                < modelSwarm.getTriggerLikelihood())))  {
            
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
                
        triggerTick = Globals.env.getCurrentTime() + 
          uniformTrigTime.getUnsignedSample();
        trap = modelSwarm.getMousetrapAtX$Y (xTrigger, yTrigger);
                
        if (trap != null) {
          ((MousetrapModelSwarmImpl) modelSwarm).getStats().addOneBall();
          modelSwarm.scheduleTriggerAt$For(triggerTick, trap);
        }
      }
    }
  }
}


/**
 * This class is the `main' function of the Mousetrap application */
class StartMousetrap
{
  public static void nag (String s)
  {
    System.out.println ( s);
  }
  public static void main (String[] args)
  {
    Globals.env.initSwarm(args);

    if (Globals.env.guiFlag) {
        
      MousetrapObserverSwarmImpl topLevelSwarm 
        = new MousetrapObserverSwarmImpl(Globals.env.globalZone);
        
      Globals.env.setWindowGeometryRecordName (topLevelSwarm);
      topLevelSwarm.buildObjects ();
      topLevelSwarm.buildActions ();
      topLevelSwarm.activateIn (null);
      topLevelSwarm.go ();
        
    }
    else {
      MousetrapBatchSwarmImpl topLevelSwarm 
        = new MousetrapBatchSwarmImpl((ZoneImpl)Globals.env.globalZone);
      topLevelSwarm.buildObjects();
      topLevelSwarm.buildActions();
      topLevelSwarm.activateIn(null);
      topLevelSwarm.go();
    }
  }
}






 
 
