package agent2d;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;

abstract class DirectedAgent2d extends Agent2d {
  int xoffset, yoffset;

  DirectedAgent2d (Zone aZone,
                   Grid2d world,
                   int x, int y,
                   int scatter, int size,
                   double resistProbabilityMean, double resistProbabilityDeviation,
                   int energyMean, int energyDeviation) {
    super (aZone, world, x, y, scatter, size,
           resistProbabilityMean, resistProbabilityDeviation,
           energyMean, energyDeviation);
  }

  public void setOffsets () {
    if (direction == 90) {
      xoffset = 0;
      yoffset = 1;
    } else if (direction == 270) {
      xoffset = 0;
      yoffset = -1;
    } else if (direction == 180) {
      xoffset = -1;
      yoffset = 0;
    } else if (direction == 0) {
      xoffset = 1;
      yoffset = 0;
    } else {
      if (direction < 90) {
        xoffset = 1;
        yoffset = 1;
      } else if (direction < 180) {
        xoffset = -1;
        yoffset = 1;
      } else if (direction < 270) {
        xoffset = -1;
        yoffset = -1;
      } else {
        xoffset = 1;
        yoffset = -1;
      }
    }
  }

  public void moveDirection () {
    setOffsets ();
    moveAgent (xoffset, yoffset);
  }

  public Object drawSelfOn (Raster r) {
    double theta = Math.toRadians ((double) direction);
    int xo = (int) Math.round (size * Math.cos (theta));
    int yo = (int) Math.round (size * Math.sin (theta));

    r.ellipseX0$Y0$X1$Y1$Width$Color (x - size, y - size,
                                      x + size, y + size,
                                      1, color);
    r.lineX0$Y0$X1$Y1$Width$Color (x, y, x + xo, y + yo, 1, color);

    return this;
  }
}
