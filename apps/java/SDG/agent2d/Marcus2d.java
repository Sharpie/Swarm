package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

import swarm.Globals;

public class Marcus2d extends Agent2d {
  public Marcus2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y);
  }

  public void stepAgent () {
    world.putObject$atX$Y (null, x, y);
    x += Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2);
    y += Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2);
    world.putObject$atX$Y (this, x, y);
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, (byte) 0);
    return this;
  }
}
