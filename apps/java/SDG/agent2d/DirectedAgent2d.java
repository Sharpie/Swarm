package agent2d;
import swarm.defobj.Zone;
import swarm.space.Grid2d;

abstract class DirectedAgent2d extends Agent2d {
  int xoffset, yoffset;

  DirectedAgent2d (Zone aZone,
                  Grid2d world,
                  int x, int y,
                  int scatter,
                  double resistProbabilityMean, double resistProbabilityDeviation,
                  int energyMean, int energyDeviation) {
    super (aZone, world, x, y, scatter,
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
}
