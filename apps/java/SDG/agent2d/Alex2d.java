package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

import java.util.Hashtable;

import swarm.Selector;
import swarm.Globals;

import ObserverSwarm;

public class Alex2d extends SocialAgent2d {
  Hashtable people;

  class Location {
    int x;
    int y;

    Location (int x, int y) {
      this.x = x;
      this.y = y;
    }
  }

  public Alex2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y, 2, .2, .1, 40, 20, 4);

    people = new Hashtable (10);
  }
  
  public void stepSocialAgent (Agent2d neighbor) {
    if (neighbor != null) {
      Location location = (Location) people.get (neighbor);

      if (location != null)
        {
          System.out.println ("Updating neighbor " + neighbor);
          location.x = neighbor.x;
          location.y = neighbor.y;
        }
      else
        {
          System.out.println ("New neighbor " + neighbor);
          location = new Location (neighbor.x, neighbor.y);
          people.put (neighbor, location);
        }
    }
    randomWalk ();
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, ObserverSwarm.AlexTourColor);
    return this;
  }
}
