package agent2d;
import swarm.defobj.Zone;
import swarm.gui.Raster;

import swarm.random.NormalDist;
import swarm.random.NormalDistImpl;
import swarm.random.BernoulliDist;
import swarm.random.BernoulliDistImpl;

import swarm.Globals;

import Organization;

abstract class DirectedAgent2d extends Agent2d {
    int xoffset, yoffset;
    NormalDist resistProbabilityDistribution;
    double resistProbability = 0.0;
    private BernoulliDist bernoulliDist =
        new BernoulliDistImpl (getZone (), Globals.env.randomGenerator, .5);
    boolean frobbed, resisting;
    int direction;

    DirectedAgent2d (Zone aZone,
                     Organization org,
                     int x, int y,
                     int scatter, int size,
                     double resistProbabilityMean, 
                     double resistProbabilityDeviation, 
                     int energyMean, int energyDeviation) {
        super (aZone, org, x, y, scatter, size, energyMean, energyDeviation);
        this.resistProbabilityDistribution =
            new NormalDistImpl (aZone,
                                Globals.env.randomGenerator,
                                resistProbabilityMean,
                                resistProbabilityDeviation);
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
    resize();
    r.ellipseX0$Y0$X1$Y1$Width$Color (x - size, y - size,
                                      x + size, y + size,
                                      thickness, color);
    r.lineX0$Y0$X1$Y1$Width$Color (x, y, x + xo, y + yo, thickness, color);

    return this;
  }

  public void sampleResistProbability () {
    double prob;

    prob = resistProbabilityDistribution.getDoubleSample ();
    if (prob > 1.0)
      prob = 1.0;
    else if (prob < 0)
      prob = 0.0;
    resistProbability = prob;
  }

  public boolean frob (int direction) {
    frobbed = true;
    if (!bernoulliDist.getSampleWithProbability (resistProbability)
        || energy == 0) {
      resisting = false;
      System.out.println(name + ":         Wow!!!  Wiggling dots!");
      energy += 5;
      this.direction = direction;
    } else {
      resisting = true;
      System.out.println (name + ": .......working........working......" + 
                          " Energy: " + energy);
      energy -= 5;
    }
    return resisting;
  }

  public void clearFrobStatus () {
    resisting = false;
    frobbed = false;
  }
}
