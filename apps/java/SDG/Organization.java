import swarm.objectbase.SwarmImpl;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

public abstract class Organization extends SwarmImpl {
  Grid2d world;
  int xsize, ysize;
  
  public Organization (Zone aZone, int xsize, int ysize) {
    super (aZone);
    this.xsize = xsize;
    this.ysize = ysize;
    world = new Grid2dImpl (getZone (), xsize, ysize);
  }

  public Grid2d getWorld () {
    return world;
  }
}
