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

public class MousetrapModelSwarmImpl extends SwarmImpl
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
   * MousetrapModelSwarm constructor: since we are only interested in
   * subclassing from the `USING' phase object, this constructor does
   * the work of the createBegin, createEnd methods in Objective C */
  public MousetrapModelSwarmImpl (ZoneImpl aZone)
  {
    super(aZone);
    
    // in Objective C implementation, this normally goes in a
    // `createBegin' method
    
    EmptyProbeMapCImpl iepm;
    EmptyProbeMapImpl fepm = new EmptyProbeMapImpl();
    
    gridSize = 50;
    triggerLikelihood = 1.0;
    numberOutputTriggers = 2;
    maxTriggerDistance = 4;
    maxTriggerTime = 16;
    trapDensity = 1.0;
    
    nag ("Model: Custom probe map");
    
    //    CustomProbeMapImpl probeMap = new CustomProbeMapImpl
    //      ((ZoneImpl) aZone, this.getClass(), "gridSize", 
    //      "triggerLikelihood", "numberOutputTriggers", "maxTriggerDistance",
    //      "maxTriggerTime", "trapDensity", ":", null);
        
    iepm = new EmptyProbeMapCImpl (fepm);
    nag ("Model: Empty probe map create begin");
    iepm.createBegin (aZone);
    nag ("Model: probe this");
    iepm.setProbedClass (this.getClass());
    nag ("Model: Create End");
    fepm = (EmptyProbeMapImpl) iepm.createEnd();
    nag ("add probe");
    fepm.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("gridSize", this.getClass()));
    fepm.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("triggerLikelihood", this.getClass()));
    fepm.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("numberOutputTriggers", this.getClass()));
    fepm.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("maxTriggerDistance", this.getClass()));
    fepm.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("maxTriggerTime", this.getClass()));
    fepm.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass 
       ("trapDensity", this.getClass()));

    Globals.env.probeLibrary.setProbeMap$For (fepm, this.getClass());

    // in Objective C implementation, this normally goes in a
    // `createEnd' method

    PMMLCG1genCImpl irg;
    UniformDoubleDistCImpl iudd;

    randomGenerator = new PMMLCG1genImpl ();
    irg = new PMMLCG1genCImpl (randomGenerator);
    randomGenerator = 
      (PMMLCG1genImpl) irg.create$setStateFromSeed 
      (((MousetrapModelSwarmImpl) this).getZone(), 1234567890);
    uniform0to1 = new UniformDoubleDistImpl ();

    iudd = new UniformDoubleDistCImpl (uniform0to1);
    uniform0to1 = 
      (UniformDoubleDistImpl) 
      iudd.create$setGenerator$setDoubleMin$setMax 
      ( ((MousetrapModelSwarmImpl)this).getZone(), 
        randomGenerator, 0.0, 1.0 );
    
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

  public Object buildActions ()
  {
    super.buildActions();
    modelSchedule = new ScheduleImpl ((ZoneImpl)this.getZone(), true);
    scheduleTriggerAt$For (0, (Mousetrap) grid.getObjectAtX$Y 
                           (gridSize/2, gridSize/2));
    
    stats.addOneBall();
    return this;
  }
    
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
