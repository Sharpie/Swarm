// Java mousetrap application. Copyright © 1999-2000 Swarm Development Group.
// This application is distributed without any warranty; without even
// the implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.objectbase.SwarmImpl;

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;
import swarm.objectbase.ActivityControl;
import swarm.objectbase.ActivityControlImpl;
import swarm.objectbase.Swarm;
import swarm.objectbase.VarProbe;
import swarm.objectbase.EmptyProbeMapImpl;
import swarm.random.PMMLCG1gen;
import swarm.random.PMMLCG1genImpl;
import swarm.random.UniformDoubleDist;
import swarm.random.UniformDoubleDistImpl;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

/** 
 * The MousetrapModelSwarm defines the mousetrap world. All of the
 * structures specific to the model are built and scheduled here.
 * Observations on the model are built and scheduled in the
 * MousetrapObserverSwarm */
public class MousetrapModelSwarm extends SwarmImpl
{
  public int gridSize;
  public double triggerLikelihood;
  public int numberOutputTriggers;
  public int maxTriggerDistance;
  public int maxTriggerTime;
  public double trapDensity;
  
  public Schedule modelSchedule;
  
  public MousetrapStatistics stats;
  public Grid2d grid;
  
  public ActivityControl modelActCont;
  
  public PMMLCG1gen randomGenerator;
  public UniformDoubleDist uniform0to1;
  
  /**
   * MousetrapModelSwarm constructor: since we are only interested
   * in subclassing from the `USING' phase object, this constructor
   * does the work of the createBegin, createEnd methods in Objective
   * C */
  public MousetrapModelSwarm (Zone aZone) {
    super (aZone);
    
    gridSize = 50;
    triggerLikelihood = 1.0;
    numberOutputTriggers = 2;
    maxTriggerDistance = 4;
    maxTriggerTime = 16;
    trapDensity = 1.0;

    // Use a Java local class to generate the ProbeMap instance
    class MousetrapModelProbeMap extends EmptyProbeMapImpl {
        private VarProbe probeVariable (String name) {
            return
                Globals.env.probeLibrary.getProbeForVariable$inClass
                (name, MousetrapModelSwarm.this.getClass ());
        }
        private void add (String name) {
            addProbe (probeVariable (name));
        }
        public MousetrapModelProbeMap (Zone _aZone, Class aClass) {
            super (_aZone, aClass);
            add ("triggerLikelihood");
            add ("numberOutputTriggers");
            add ("maxTriggerDistance");
            add ("maxTriggerTime");
            add ("trapDensity");
        }
    }

    Globals.env.probeLibrary.setProbeMap$For 
      (new MousetrapModelProbeMap (aZone, getClass ()), getClass ());

    randomGenerator = new PMMLCG1genImpl (aZone, 1234567890);
    uniform0to1 = new UniformDoubleDistImpl (aZone, randomGenerator, 0.0, 1.0);
  }

  public MousetrapStatistics getStats () {
    return stats;
  }

  public int getGridSize () {
    return gridSize;
  }

  public double getTriggerLikelihood () {
    return triggerLikelihood;
  }

  public int getNumberOutputTriggers () {
    return numberOutputTriggers;
  }
  
  public int getMaxTriggerDistance () {
    return maxTriggerDistance;
  }

  public int getMaxTriggerTime () {
    return maxTriggerTime;
  }

  public Grid2d getWorld () {
    return grid;
  }
  
  public Object getSchedule () {
    return modelSchedule;
  }

  public Mousetrap getMousetrapAtX$Y (int x, int y) {
    return (Mousetrap) grid.getObjectAtX$Y (x, y);
  }

  /**
   * Building of the model objects. We use various parameters set via
   * the constructor, to choose how to create things. */
  public Object buildObjects () {
    int x, y;
    
    super.buildObjects();
    
    stats = new MousetrapStatistics ();
    grid = new Grid2dImpl (getZone (), gridSize, gridSize);

    for (y = 0; y < gridSize; y++)
      for (x = 0; x < gridSize; x++)
        if (trapDensity >= 1.0 || 
            (float) (uniform0to1.getDoubleSample ()) < trapDensity) {
          Mousetrap aMousetrap;
                    
          aMousetrap = new Mousetrap (this, x, y, randomGenerator);
          grid.putObject$atX$Y (aMousetrap, x, y);
        }
    return this;
  }

 /** 
  * Here is where the model schedule is built, the data structures
  * that define the simulation of time in the model. Here, we
  * implement *dynamic scheduling* Here is where mousetrap differs
  * from time-step models like heatbugs.  Mousetrap uses a
  * discrete-event time-update, so we don't create a regularly
  * repeated ActionGroupImpl and put it on a schedule.  Instead, we
  * merely create an empty ScheduleImpl, and let it know that once an
  * action has been executed, it is to be dropped from the schedule,
  * by using the (aZone, autoDrop) constructor for ScheduleImpl */
  public Object buildActions ()
  {
    super.buildActions();
    modelSchedule = new ScheduleImpl (getZone (), true);
    scheduleTriggerAt$For (0, (Mousetrap) grid.getObjectAtX$Y 
                           (gridSize / 2, gridSize / 2));
    
    stats.addOneBall();
    return this;
  }

  /** 
   * This is how new actions get added to the schedule. When a
   * mousetrap triggers, it randomly picks some other "nearby"
   * mousetraps to trigger. "Triggering" mousetraps simply means to
   * add a "trigger" action on them to the schedule, inserted at the
   * proper time in the future.  */
  public Object scheduleTriggerAt$For (int n, Mousetrap trap)
  {
    try {
      modelSchedule.at$createActionTo$message 
        (n, trap, new Selector (trap.getClass (), "trigger", false));
    } catch (Exception e) { 
      System.out.println ("Exception:" + e.getMessage ());
    }
    return this;
  }
  
  /**
   * Now set up the model's activation. swarmContext indicates where
   * we're being started in - typically, this model is run as a
   * subswarm of an observer swarm.  */
  public Activity activateIn (Swarm swarmContext)
  {
    super.activateIn (swarmContext);
    modelSchedule.activateIn (this);
   
    modelActCont = new ActivityControlImpl (getZone ());
    modelActCont.attachToActivity (getActivity ());

    modelActCont.setDisplayName ("Model Swarm Controller");

    Globals.env.createArchivedProbeDisplay (modelActCont, "modelActCont");
        
    return getActivity ();
  }
}
