package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.defobj.Zone;
import swarm.gui.Raster;

import ObserverSwarm;
import Organization;

import swarm.Selector;
import swarm.Globals;

public class Glen2d extends SocialAgent2d {
  public Glen2d (Zone aZone, Organization org, int x, int y) {
    super (aZone, org, x, y, 4, 6);
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
