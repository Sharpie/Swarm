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

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

public class SDG extends Organization {
  final static int userCount = 10;
  Grid2d world;
  Agent2d mgd, gepr, alex;
  Agent2d user1, user2, user3;
  List userList;

  public SDG (Zone aZone) {
    super (aZone);
  }

  public Object buildObjects () {
    world = new Grid2dImpl (getZone (), 100, 100);
    alex = new Alex2d (getZone (), world, 20, 20);
    gepr = new Glen2d (getZone (), world, 10, 10);
    mgd = new Marcus2d (getZone (), world, 0, 0);

    userList = new LinkedList ();

    for (int i = 0; i < userCount; i++) {
      int x = Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, world.getSizeX () - 1);
      int y = Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, world.getSizeY () - 1);
      int scatter = Globals.env.uniformIntRand.getIntegerWithMin$withMax (1, 5);
      double resistProbabilityMean =
        Globals.env.uniformDblRand.getDoubleWithMin$withMax (0, 1);
      double resistProbabilityDeviation =
        Globals.env.uniformDblRand.getDoubleWithMin$withMax (0, 1);
      int energyMean = Globals.env.uniformIntRand.getIntegerWithMin$withMax (1, 50);
      int energyDeviation = Globals.env.uniformIntRand.getIntegerWithMin$withMax (1, 50);
      
      userList.add (new User2d (getZone (), world, x, y,
                                scatter,
                                resistProbabilityMean,
                                resistProbabilityDeviation,
                                energyMean,
                                energyDeviation));
    }
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
    
    Iterator iterator = userList.iterator ();
    while (iterator.hasNext ())
      ((Agent2d) iterator.next ()).activateIn (this);

    return getActivity ();
  }
}
