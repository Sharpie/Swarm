import swarm.activity.Activity;
import swarm.objectbase.Swarm;
import swarm.Globals;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

import agent2d.Agent2d;
import agent2d.Marcus2d;
import agent2d.Glen2d;
import agent2d.Alex2d;
import agent2d.User2d;

public class SDG extends Organization {
  Grid2d world;
  Agent2d mgd, gepr, alex;
  Agent2d user1, user2;

  public SDG (Zone aZone) {
    super (aZone);
  }

  public Object buildObjects () {
    world = new Grid2dImpl (getZone (), 100, 100);
    mgd = new Marcus2d (getZone (), world, 0, 0);
    gepr = new Glen2d (getZone (), world, 10, 10);
    alex = new Alex2d (getZone (), world, 20, 20);
    user1 = new User2d (getZone (), world, 30, 30);
    user2 = new User2d (getZone (), world, 40, 40);

    return this;
  }

  public Grid2d getWorld () {
    return world;
  }

  public Activity activateIn (Swarm swarmContext) {
    super.activateIn (swarmContext);

    mgd.activateIn (this);
    gepr.activateIn (this);
    alex.activateIn (this);
    user1.activateIn (this);
    user2.activateIn (this);
    return getActivity ();
  }
}
