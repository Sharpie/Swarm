package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;
import swarm.random.NormalDist;
import swarm.random.NormalDistImpl;
import swarm.random.BernoulliDist;
import swarm.random.BernoulliDistImpl;

import swarm.Globals;

public class Agent2d extends SwarmImpl {
  public int x, y;
  Grid2d world;

  double resistProbabilityMean, resistProbabilityDeviation;
  NormalDist resistProbabilityDistribution;
  NormalDist energyDistribution;
  double resistProbability = 0.0;
  BernoulliDist bernoulliDist =
    new BernoulliDistImpl (getZone (), Globals.env.randomGenerator, .5);
  boolean frobbed, resisting;
  byte color;
  int direction, energy;
  int scatter;

  public Agent2d () { }
  public Agent2d (Zone aZone,
                  Grid2d world,
                  int x, int y,
                  int scatter,
                  double resistProbabilityMean, double resistProbabilityDeviation,
                  int energyMean, int energyDeviation) {
    super (aZone);
    this.x = x;
    this.y = y;
    this.world = world;
    this.scatter = scatter;
    this.resistProbabilityDistribution =
      new NormalDistImpl (aZone,
                          Globals.env.randomGenerator,
                          resistProbabilityMean,
                          resistProbabilityDeviation);
    this.energyDistribution =
      new NormalDistImpl (aZone,
                          Globals.env.randomGenerator,
                          energyMean,
                          energyDeviation);
    world.putObject$atX$Y (this, x, y);
  }
  
  Agent2d getAgent (int xpos, int ypos) {
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
  
  public double sampleResistProbability () {
    double prob;

    do {
      prob = resistProbabilityDistribution.getDoubleSample ();
    } while (prob < 0.0 || prob > 1.0);
    return prob;
  }

  public int sampleEnergy () {
    return Math.abs ((int) energyDistribution.getDoubleSample ());
  }

  public void randomWalk () {
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-scatter, scatter),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-scatter, scatter));         
  }

  public boolean frob (int direction) {
    frobbed = true;
    if (!bernoulliDist.getSampleWithProbability (resistProbability)
        || energy == 0) {
      resisting = false;
      this.direction = direction;
    } else {
      resisting = true;
      System.out.println (this + " expending energy (" + energy + ")");
      energy--;
    }
    return resisting;
  }
  
  public void clearStatus () {
    resisting = false;
    frobbed = false;
  }

  public Object drawSelfOn (Raster r) {
    r.drawPointX$Y$Color (x, y, color);
    return this;
  }
}
