package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

import swarm.Globals;

public class Glen2d extends Agent2d {
  public Glen2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y);
  }

  public void stepAgent () {
    world.putObject$atX$Y (null, x, y);
    x += Globals.env.uniformIntRand.getIntegerWithMin$withMax (-3, 3);
    y += Globals.env.uniformIntRand.getIntegerWithMin$withMax (-3, 3);
    world.putObject$atX$Y (this, x, y);
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, (byte) 1);
    return this;
  }
}
