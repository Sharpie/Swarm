import swarm.activity.Activity;
import swarm.objectbase.Swarm;
import swarm.Globals;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

import agent2d.Agent2d;
import agent2d.Marcus2d;
import agent2d.Glen2d;

public class SDG extends Organization {
  Grid2d world;
  Agent2d mgd, gepr;

  public SDG (Zone aZone) {
    super (aZone);
  }

  public Object buildObjects () {
    world = new Grid2dImpl (getZone (), 100, 100);
    mgd = new Marcus2d (getZone (), world, 0, 0);
    gepr = new Glen2d (getZone (), world, 10, 10);

    return this;
  }

  public Grid2d getWorld () {
    return world;
  }

  public Activity activateIn (Swarm swarmContext) {
    super.activateIn (swarmContext);

    mgd.activateIn (this);
    gepr.activateIn (this);
    return getActivity ();
  }
}
