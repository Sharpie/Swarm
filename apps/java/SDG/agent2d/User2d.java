package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.defobj.Zone;
import swarm.gui.Raster;

import swarm.Selector;
import swarm.Globals;

import ObserverSwarm;
import Organization;

public class User2d extends DirectedAgent2d {
  Schedule schedule;

  void newEffort () {
    direction = Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, 359);
    sampleEnergy ();
  }

  public User2d (Zone aZone, Organization org,
                 String myName,
                 int x, int y,
                 int scatter,
                 double resistanceProbabilityMean,
                 double resistanceProbabilityDeviation,
                 int energyMean, int energyDeviation) {

    super (aZone, org, x, y, scatter, 2,
           resistanceProbabilityMean, resistanceProbabilityDeviation,
           energyMean, energyDeviation);
    name = myName;
    sampleResistProbability ();
    newEffort ();
    schedule = new ScheduleImpl (aZone, 1);

    try {
      schedule.at$createActionTo$message
        (0,
         this,
         new Selector (getClass (), "stepAgent", false));
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
  }

  public Activity activateIn (Swarm context) {
    super.activateIn (context);

    schedule.activateIn (this);
    return getActivity ();
  }

  public void stepAgent () {
    if (energy < 0) sampleEnergy();
    if (frobbed && !resisting)
      color = ObserverSwarm.UserListenColor;
    else
      color = resisting ? ObserverSwarm.UserResistColor : ObserverSwarm.UserTourColor;
    moveDirection ();
    if (energy == 0)
      newEffort ();
    else
      energy--;
    clearFrobStatus ();
  }
}
