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
    //public SwarmEnvironment se;
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
    int x,y;
    // Grid2dCImpl ig2d;

    super.buildObjects();
    
    stats = new MousetrapStatistics();
    //stats.se = se;
    grid = new Grid2dImpl((ZoneImpl)this.getZone(), gridSize, gridSize);

    //    nag ((new Integer(gridSize).toString()));
    //ig2d = new Grid2dCImpl (grid);
    //ig2d.createBegin (this.getZone());
    //ig2d.setSizeX$Y (gridSize, gridSize);
    //    nag ("pre grid");
    //grid = (Grid2dImpl) ig2d.createEnd();

    for (y = 0; y< gridSize; y++)
      for (x = 0; x< gridSize; x++)
	if (trapDensity >= 1.0 || 
	    (float)(uniform0to1.getDoubleSample()) < trapDensity)
	  {
	    Mousetrap aMousetrap;
	    
	    aMousetrap = new Mousetrap (this, x, y, randomGenerator);
	    grid.putObject$atX$Y (aMousetrap, x, y);
	    // aMousetrap.se = se;
	  }
    return this;
  }

  public Object buildActions ()
  {
    ScheduleCImpl isc;
    //    nag ("build actions bre");
    super.buildActions();
    modelSchedule = new ScheduleImpl ((ZoneImpl)this.getZone(), true);
    //isc = new ScheduleCImpl (modelSchedule);
    //isc.createBegin (this.getZone());
    //isc.setAutoDrop (true);
    //modelSchedule = (ScheduleImpl) isc.createEnd ();
  
    scheduleTriggerAt$For (0, (Mousetrap) grid.getObjectAtX$Y (gridSize/2, 
                                                               gridSize/2));
    
    stats.addOneBall();
    return this;
  }

  public Object scheduleTriggerAt$For (int n, Mousetrap trap)
  {
    Selector slct;
    //    nag ("scheduletrigger");
    try {
      if (trap == null);
      //	nag ("Mousetrap is NOT ok");
      slct = new Selector (trap.getClass(), "trigger", false);
      if (slct == null);
      //      nag ("Selector is NOT ok");
      modelSchedule.at$createActionTo$message (n, trap, slct);
      //    nag ("Schedule stuff for mousetrap");
    } catch (Exception e) { 
        System.out.println ("Exception:" + e.getMessage());
    }
    return this;
  }

  public Object activateIn (Object swarmContext)
  {
    ActivityControlCImpl actCon;
    super.activateIn (swarmContext);
    modelSchedule.activateIn (this);
   
    modelActCont = new ActivityControlImpl ();
    actCon = new ActivityControlCImpl (modelActCont);
    actCon.createBegin (this.getZone());
    actCon.attachToActivity (this.getActivity());
    modelActCont = (ActivityControlImpl) actCon.createEnd();

    //    modelActCont.setDisplayName ("Model Swarm Controller");

    return this.getActivity();
    
  }
}
