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
  
  public ScheduleImpl modelSchedule;
  
  public MousetrapStatistics stats;
  public Grid2dImpl grid;
  
  public ActivityControlImpl modelActCont;
  
  public PMMLCG1genImpl randomGenerator;
  public UniformDoubleDistImpl uniform0to1;
    
  public void nag (String s)
  {
    System.out.println (this.getClass().getName() + ":" + s);
    System.out.flush ();
  }

  /**
   * MousetrapModelSwarm constructor: since we are only interested
   * in subclassing from the `USING' phase object, this constructor
   * does the work of the createBegin, createEnd methods in Objective
   * C */
  public MousetrapModelSwarm (ZoneImpl aZone)
  {
    super(aZone);
    
    // in Objective C implementation, this normally goes in a
    // `createBegin' method
    
    gridSize = 50;
    triggerLikelihood = 1.0;
    numberOutputTriggers = 2;
    maxTriggerDistance = 4;
    maxTriggerTime = 16;
    trapDensity = 1.0;
    
    nag ("Model: EmptyProbeMap");
    EmptyProbeMapImpl probeMap = new EmptyProbeMapImpl(aZone, this.getClass());

    nag ("add probe");
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("gridSize", this.getClass()));
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("triggerLikelihood", this.getClass()));
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("numberOutputTriggers", this.getClass()));
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("maxTriggerDistance", this.getClass()));
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("maxTriggerTime", this.getClass()));
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("trapDensity", this.getClass()));

    Globals.env.probeLibrary.setProbeMap$For (probeMap, this.getClass());

    // in Objective C implementation, the following normally goes in a
    // `createEnd' method

    randomGenerator = 
        new PMMLCG1genImpl (aZone, 1234567890);

    uniform0to1 = new UniformDoubleDistImpl (aZone, randomGenerator, 0.0, 1.0 );
    
  }

  public MousetrapStatistics getStats ()
  {
    return stats;
  }

  public int getGridSize ()
  {
    return gridSize;
  }

  public double getTriggerLikelihood ()
  {
    return triggerLikelihood;
  }

  public int getNumberOutputTriggers ()
  {
    return numberOutputTriggers;
  }
  
  public int getMaxTriggerDistance ()
  {
    return maxTriggerDistance;
  }

  public int getMaxTriggerTime ()
  {
    return maxTriggerTime;
  }

  public Object getWorld ()
  {
    return grid;
  }
  
  public Object getSchedule ()
  {
    return modelSchedule;
  }

  public Mousetrap getMousetrapAtX$Y (int x, int y)
  {
    return (Mousetrap) grid.getObjectAtX$Y (x,y);
  }

  /**
   * Building of the model objects. We use various parameters set via
   * the constructor, to choose how to create things. */
  public Object buildObjects()
  {
    int x, y;

    super.buildObjects();
    
    stats = new MousetrapStatistics();
    grid = new Grid2dImpl((ZoneImpl)this.getZone(), gridSize, gridSize);

    for (y = 0; y< gridSize; y++)
      for (x = 0; x< gridSize; x++)
        if (trapDensity >= 1.0 || 
            (float)(uniform0to1.getDoubleSample()) < trapDensity) {
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
    modelSchedule = new ScheduleImpl ((ZoneImpl)this.getZone(), true);
    scheduleTriggerAt$For (0, (Mousetrap) grid.getObjectAtX$Y 
                           (gridSize/2, gridSize/2));
    
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
        (n, trap, new Selector (trap.getClass(), "trigger", false));
    } catch (Exception e) { 
      System.out.println ("Exception:" + e.getMessage());
    }
    return this;
  }

  /**
   * Now set up the model's activation. swarmContext indicates where
   * we're being started in - typically, this model is run as a
   * subswarm of an observer swarm.  */
  public Object activateIn (Object swarmContext)
  {
    super.activateIn (swarmContext);
    modelSchedule.activateIn (this);
   
    modelActCont = new ActivityControlImpl ((ZoneImpl)this.getZone());
    modelActCont.attachToActivity (this.getActivity());

    //    modelActCont.setDisplayName ("Model Swarm Controller");
        
    return this.getActivity();
  }
}
