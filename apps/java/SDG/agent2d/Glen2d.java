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

public class Glen2d extends SocialAgent2d {
  public Glen2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y, 4, 4, .5, .25, 100, 50, 6);
  }
  
  public void stepSocialAgent (Agent2d neighbor) {
    if (neighbor != null) {
      if (!neighbor.frob (Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, 359))) {
        moveAdjacentToNeighbor (neighbor);
        color = ObserverSwarm.GlenAttackColor;
        return;
      }
    }
    color = ObserverSwarm.GlenTourColor;
    randomWalk ();
  }
  
}
