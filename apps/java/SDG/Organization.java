package SDG;

import swarm.objectbase.SwarmImpl;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

import swarm.activity.Schedule;
import swarm.activity.ScheduleC;
import swarm.activity.ScheduleImpl;
import swarm.activity.ScheduleCImpl;
import swarm.activity.Activity;
import swarm.objectbase.Swarm;

public abstract class Organization extends SwarmImpl {
  Grid2d world;
  int xsize, ysize;
  Schedule reaper;
  
  public Organization (Zone aZone, int xsize, int ysize) {
    super (aZone);
    this.xsize = xsize;
    this.ysize = ysize;
    world = new Grid2dImpl (getZone (), xsize, ysize);
    reaper = new ScheduleImpl (aZone);
  }

  public Grid2d getWorld () {
    return world;
  }

  public Schedule getReaper () {
    return reaper;
  }

  public Activity activateIn (Swarm swarmContext) {
    super.activateIn (swarmContext);
    
    reaper.activateIn (this);
    return getActivity ();
  }
}
