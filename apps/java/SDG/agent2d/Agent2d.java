package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

public class Agent2d extends SwarmImpl {
  int x, y;
  Grid2d world;

  Agent2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone);
    this.x = x;
    this.y = y;
    this.world = world;
    world.putObject$atX$Y (this, x, y);
  }
  public Object drawSelfOn (Raster r) {
    return this;
  }

}
