package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

import ObserverSwarm;

import swarm.Selector;
import swarm.Globals;

public class Glen2d extends Agent2d {
  Schedule schedule;
  boolean attacking;

  public Glen2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y, .5, .25, 100, 50);

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
    Agent2d neighbor = getNeighbor (6);
    if (neighbor != null) {
      if (!neighbor.frob (Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, 359))) {
        moveAgent (neighbor.x - x, neighbor.y - y);
        attacking = true;
        return;
      }
    }
    attacking = false;
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-4, 4),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-4, 4));
  }
  
  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, attacking ? ObserverSwarm.GlenAttackColor : ObserverSwarm.GlenTourColor);
    return this;
  }
}
