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
import swarm.Globals;

import ObserverSwarm;

public class User2d extends Agent2d {
  Schedule schedule;

  public User2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y, .6, .1, 10, 5);

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
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-1, 1),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-1, 1));
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, resisting ? ObserverSwarm.UserResistColor : ObserverSwarm.UserTourColor);
    return this;
  }
}
