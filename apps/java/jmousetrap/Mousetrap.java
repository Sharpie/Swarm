// Java mousetrap application. Copyright � 1999 Santa Fe Institute.
// This application is distributed without any warranty; without even
// the implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.random.UniformDoubleDist;
import swarm.random.UniformDoubleDistImpl;
import swarm.random.UniformUnsignedDist;
import swarm.random.UniformUnsignedDistImpl;
import swarm.random.UniformIntegerDist;
import swarm.random.UniformIntegerDistImpl;
import swarm.gui.ZoomRasterImpl;
import swarm.Globals;

/**
 This class implements a `Mousetrap' agent.  Mousetraps that live in
 fixed positions on a 2D lattice. Each mousetrap has N "ping-pong"
 balls.  When "triggered" (hit by another ping-pong ball) it releases
 its ping-pong balls into the air, which hit other mousetraps and
 trigger them.  Tossing ping-pong balls into the air to trigger other
 mousetraps is accomplished by picking "nearby" mousetraps, and
 arranging for them to be sent trigger messages in the "near" future.  */
public class Mousetrap {
  public int xCoord;
  public int yCoord;
  public boolean triggered;
  public ZoomRasterImpl displayWidget;
  public MousetrapModelSwarm modelSwarm;
  public UniformDoubleDist uniform0to1;
  public UniformIntegerDist uniformRadius;
  public UniformUnsignedDist uniformTrigTime;

  public Mousetrap (MousetrapModelSwarm s, int x, int y,
                    Object randGenerator) {
    int maxD;
    
    modelSwarm = s;
    xCoord = x;
    yCoord = y;
    
    maxD = modelSwarm.getMaxTriggerDistance();
    uniform0to1 = new  UniformDoubleDistImpl 
      (modelSwarm.getZone (), randGenerator, 0.0, 1.0);
    
    uniformRadius = new UniformIntegerDistImpl     
      (modelSwarm.getZone (), randGenerator, -maxD, maxD);
    
    uniformTrigTime =  new UniformUnsignedDistImpl 
      (modelSwarm.getZone (), randGenerator, 1, 
       modelSwarm.getMaxTriggerTime ());
  }

  /**
   * Here we record who to display ourselves on.
   */
  public Object setDisplayWidget (ZoomRasterImpl w) {
    displayWidget = w;
    return this;
  }

  public void noMethod () {
  }

  /**
   * The crucial step for a Mousetrap (equivalent to "step" for a Heatbug)  
   */
  public void trigger ()
  {
    int n, xTrigger, yTrigger;
    int triggerTick;

    modelSwarm.getStats ().removeOneBall ();

    if (!triggered
        && ((modelSwarm.getTriggerLikelihood () >= 1)
            || ((float) uniform0to1.getDoubleSample() 
                < modelSwarm.getTriggerLikelihood())))  {
            
      int size;
      triggered = true;
      modelSwarm.getStats ().addOneTriggered ();
            
      if (displayWidget != null)
        displayWidget.drawPointX$Y$Color (xCoord, yCoord, (byte) 2);
      size = modelSwarm.getGridSize();

      for (n = modelSwarm.getNumberOutputTriggers (); n>0; n--) {
        Mousetrap trap;
        xTrigger = 
          (xCoord + size + uniformRadius.getIntegerSample ()) % size;
        yTrigger = 
          (yCoord + size + uniformRadius.getIntegerSample ()) % size;
                
        triggerTick = Globals.env.getCurrentTime () + 
          uniformTrigTime.getUnsignedSample ();
        trap = modelSwarm.getMousetrapAtX$Y (xTrigger, yTrigger);
                
        if (trap != null) {
          modelSwarm.getStats ().addOneBall ();
          modelSwarm.scheduleTriggerAt$For (triggerTick, trap);
        }
      }
    }
  }
}

 
