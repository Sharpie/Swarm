package SDG.agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.defobj.Zone;
import swarm.gui.Raster;

import swarm.Selector;

import SDG.Organization;

public abstract class SocialAgent2d extends Agent2d {
  Schedule schedule;
  
  public SocialAgent2d (Zone aZone, Organization org,
                        int x, int y,
                        int scatter, int size, int energyMean,
                        int energyDeviation) {
    super (aZone, org, x, y, scatter, size, energyMean, energyDeviation);

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
    stepSocialAgent (getNeighbor (size));
  }

  public void stepSocialAgent (Agent2d neighbor) {
      int energyMean = (int)energyDistribution.getMean();
      if (energy < 0) sampleEnergy();
      if (energy > 10*energyMean) energy = 10*energyMean;
  }

  public Object drawSelfOn (Raster r) {
    resize();
    super.drawSelfOn (r);
    r.rectangleX0$Y0$X1$Y1$Width$Color
      (x - size, y - size, x + size, y + size, 1, color);
    return this;
  }

  public boolean frob (int direction) {
    System.out.println (name + ": Of course WE can do that!");
    energy += 10;
    return false;
  }
}

