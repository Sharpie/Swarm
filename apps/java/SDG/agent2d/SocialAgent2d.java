package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

import swarm.Selector;

public abstract class SocialAgent2d extends Agent2d {
  int vision;
  Schedule schedule;
  
  public SocialAgent2d (Zone aZone, Grid2d world,
                        int x, int y,
                        int scatter, int size,
                        double resistProbabilityMean, double resistProbabilityDeviation,
                        int energyMean, int energyDeviation,
                        int vision) {
    super (aZone, world, x, y, scatter, size,
           resistProbabilityMean, resistProbabilityDeviation,
           energyMean, energyDeviation);

    this.vision = vision;

    schedule = new ScheduleImpl (aZone, 1);

    try {
      schedule.at$createActionTo$message
        (0,
         this,
         new Selector (getClass (), "stepAgent", false));
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
  }
  
  public Activity activateIn (Swarm context) {
    super.activateIn (context);

    schedule.activateIn (this);
    return getActivity ();
  }

  public void stepAgent () {
    stepSocialAgent (getNeighbor (vision));
  }

  public void stepSocialAgent (Agent2d neighbor) {
  }
}
