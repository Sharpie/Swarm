package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

import swarm.Globals;

public class Agent2d extends SwarmImpl {
  public int x, y;
  Grid2d world;

  double resistProbabilityMean;
  double resistProbabilityDeviation;
  int resistEnergyMean;
  int resistEnergyDeviation;

  Agent2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone);
    this.x = x;
    this.y = y;
    this.world = world;
    world.putObject$atX$Y (this, x, y);
  }
  
  Agent2d getAgent (int x, int y) {
    if (x < 0)
      return null;
    else if (y < 0)
      return null;
    else if (x >= world.getSizeX ())
      return null;
    else if (y >= world.getSizeY ())
      return null;
  
    return (Agent2d) world.getObjectAtX$Y (x, y);
  }
    
  public Agent2d getNeighbor (int width) {
    int agentCount = 0;
    int xi, yi;

    for (yi = -width; yi <= width; yi++) {
      if (yi == 0)
        continue;
      for (xi = -width; xi <= width; xi++) {
        if (xi == 0)
          continue;
        if (getAgent (x + xi, y + yi) != null)
          agentCount++;
      }
    }
    if (agentCount > 0) {
      int selected =
        Globals.env.uniformIntRand.getIntegerWithMin$withMax (0,
                                                              agentCount - 1);
      Agent2d agent;
      
      agentCount = 0;
      for (yi = -width; yi <= width; yi++) {
        if (yi == 0)
          continue;
        for (xi = -width; xi <= width; xi++) {
          if (xi == 0)
            continue;
          agent = getAgent (x + xi, y + yi);
          if (agent != null && agentCount == selected)
            return agent;
        }
      }
    }
    return null;
  }
    
  public void moveAgent (int xoffset, int yoffset) {
    int newx, newy;
    world.putObject$atX$Y (null, x, y);
    newx = x;
    newy = y;
    newx += xoffset;
    newy += yoffset;
    if (newx < 0)
      newx = 0;
    else if (newx >= world.getSizeX ())
      newx = world.getSizeX () - 1;
    if (newy < 0)
      newy = 0;
    else if (newy >= world.getSizeY ())
      newy = world.getSizeY () - 1;
    x = newx;
    y = newy;
    world.putObject$atX$Y (this, x, y);
  }

  public boolean frob () {
    System.out.println ("Frobbed! " + this);
    return false;
  }

  public Object drawSelfOn (Raster r) {
    return this;
  }
}
