package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;
import swarm.activity.Schedule;
import swarm.Selector;

import Organization;

import swarm.Globals;

public class Agent2d extends SwarmImpl {
  int size;
  int scatter;

  int x, y; 
  private Grid2d world;
  byte color;
  Schedule reaper;
  Selector dropSelector;

  public Agent2d (Zone aZone, Organization org,
                  int x, int y,
                  int scatter, int size) {
    super (aZone);
    this.x = x;
    this.y = y;
    this.world = org.getWorld ();
    this.reaper = org.getReaper ();
    this.scatter = scatter;
    this.size = size;
    world.putObject$atX$Y (this, x, y);

    try {
      dropSelector = new Selector (getClass (), "dropMe", false);
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
  }
  
  private Agent2d getAgent (int xpos, int ypos) {
    if (xpos < 0)
      return null;
    else if (ypos < 0)
      return null;
    else if (xpos >= world.getSizeX ())
      return null;
    else if (ypos >= world.getSizeY ())
      return null;
  
    return (Agent2d) world.getObjectAtX$Y (xpos, ypos);
  }

  public Agent2d getNeighbor (int width) {
    int agentCount = 0;
    int xi, yi;

    for (yi = -width; yi <= width; yi++)
      if (yi != 0)
        for (xi = -width; xi <= width; xi++)
          if (xi != 0)
            if (getAgent (x + xi, y + yi) != null)
              agentCount++;
    if (agentCount > 0) {
      int selected =
        Globals.env.uniformIntRand.getIntegerWithMin$withMax (0,
                                                              agentCount - 1);
      Agent2d agent;
      
      agentCount = 0;
      for (yi = -width; yi <= width; yi++)
        if (yi != 0)
          for (xi = -width; xi <= width; xi++)
            if (xi != 0) {
              agent = getAgent (x + xi, y + yi);
              if (agent != null) {
                if (agentCount == selected)
                  return agent;
                else
                  agentCount++;
              }
            }
    }
    return null;
  }
    
  public void moveAgent (int xo, int yo) {
    int newx, newy;
    newx = x;
    newy = y;
    newx += xo;
    newy += yo;
    if (newx < 0)
      newx = 0;
    else if (newx >= world.getSizeX ())
      newx = world.getSizeX () - 1;
    if (newy < 0)
      newy = 0;
    else if (newy >= world.getSizeY ())
      newy = world.getSizeY () - 1;
    if (world.getObjectAtX$Y (newx, newy) == null) {
      world.putObject$atX$Y (null, x, y);
      x = newx;
      y = newy;
      world.putObject$atX$Y (this, x, y);
    }
  }

  public void moveAdjacentToNeighbor (Agent2d neighbor) {
    if (neighbor.x > x)
      moveAgent (neighbor.x - x - 1, neighbor.y - y);
    else if (neighbor.y > y)
      moveAgent (neighbor.x - x, neighbor.y - y - 1);
    else if (x > neighbor.x)
      moveAgent (neighbor.x - x + 1, neighbor.y - y);
    else if (y > neighbor.y)
      moveAgent (neighbor.x - x + 1, neighbor.y - y + 1);
  }

  public void randomWalk () {
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-scatter, scatter),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-scatter, scatter));         
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, color);
    return this;
  }

  public boolean frob (int direction) {
    return false;
  }

  public void dropMe () {
    Globals.env.xprint (this);
    
    // Need Swarm snapshot 2000-07-20 or greater to actually drop it
    // drop (); 
  }

  public void remove () {
    world.putObject$atX$Y (null, x, y);
    getActivity ().terminate ();
    reaper.at$createActionTo$message (Globals.env.getCurrentTime () + 1,
                                      this,
                                      dropSelector);
  }
}

