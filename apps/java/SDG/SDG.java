import swarm.activity.Activity;
import swarm.objectbase.Swarm;
import swarm.Globals;
import swarm.defobj.Zone;

import agent2d.Agent2d;
import agent2d.Marcus2d;
import agent2d.Glen2d;
import agent2d.Alex2d;
import agent2d.User2d;

import Organization;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

public class SDG extends Organization {
  public int userCount = 10;
  Agent2d mgd, gepr, alex;
  Agent2d user1, user2, user3;
  List userList;

  public SDG (Zone aZone, int xsize, int ysize, int users) {
    super (aZone, xsize, ysize);
    userCount = users;
  }

  public Object buildObjects () {
    super.buildObjects ();
    alex = new Alex2d (getZone (), this, 25, 25);
    gepr = new Glen2d (getZone (), this, 75, 75);
    mgd = new Marcus2d (getZone (), this, 50, 50);

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
      String name = (String)ObserverSwarm.nameTable.get
          (Globals.env.uniformIntRand.getIntegerWithMin$withMax
           (0, ObserverSwarm.nameTable.size()-1));
      userList.add (new User2d (getZone (), this, name, x, y,
                                scatter,
                                resistProbabilityMean,
                                resistProbabilityDeviation,
                                energyMean,
                                energyDeviation));
    }
    return this;
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
