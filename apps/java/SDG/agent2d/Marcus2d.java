package SDG.agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;
import swarm.activity.Action;
import swarm.defobj.Zone;
import swarm.gui.Raster;
import swarm.Selector;
import swarm.defobj.FArgumentsC;
import swarm.defobj.FArguments;
import swarm.defobj.FArgumentsImpl;
import swarm.defobj.FArgumentsCImpl;
import swarm.defobj.FCallImpl;

import swarm.Globals;

import SDG.ObserverSwarm;
import SDG.Organization;

public class Marcus2d extends DirectedAgent2d {
  private final static int incubationTime = 50;
  Schedule schedule;
  Selector incubateSelector;
  Selector checkWorkSelector;
  Selector xmoveSelector, ymoveSelector;
  Action xmoveNext, ymoveNext;
  int xfreq, yfreq;
  int incubationRemaining;
  boolean working;

  public Marcus2d (Zone aZone, Organization org, int x, int y) {
    super (aZone, org, x, y, 5, 4, .25, .75, 100, 10);
    name = "Marcus";
    schedule = new ScheduleImpl (aZone, true);

    try {
      Selector sel =
	new Selector (getClass (), "startIncubation", false);
      FArgumentsC fac = 
	new FArgumentsCImpl (new FArgumentsImpl ());
      fac.createBegin (getZone ());
      fac.setSelector (sel);
      fac.addInt (0);
      FArguments fa = (FArguments) fac.createEnd ();

      schedule.at$createFAction
	(0, new FCallImpl (getZone (), this, sel, fa));
      incubateSelector = 
        new Selector (getClass (), "incubate", false);
      checkWorkSelector = 
        new Selector (getClass (), "checkWork", false);
      xmoveSelector = 
        new Selector (getClass (), "xmove", false);
      ymoveSelector = 
        new Selector (getClass (), "ymove", false);
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
  }
  
  public void startIncubation (int t) {
    color = ObserverSwarm.MarcusIncubateColor;
    working = false;
    thickness = 1;
    incubationRemaining = incubationTime;
    sampleResistProbability ();
    sampleEnergy ();
    schedule.at$createActionTo$message (t, this, incubateSelector);
  }

  public void startWork (int t) {
    working = true;
    thickness = 3;
    if (frobbed)
      color = ObserverSwarm.MarcusListenColor;
    else {
      color = ObserverSwarm.MarcusNativeColor;
      direction =
        Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, 359);
    }
      
    setOffsets ();
    if (direction == 90) {
      yfreq = 1;
      xfreq = 0;
    } else if (direction == 270) {
      xfreq = 0;
      yfreq = 1;
    } else if (direction == 180) {
      xfreq = 1;
      yfreq = 0;
    } else if (direction == 0) {
      xfreq = 1;
      yfreq = 0;
    } else {
      double radians = Math.toRadians ((double) direction);
      double slope = Math.tan (radians);

      if (Math.abs (slope) >= 1.0) {
        yfreq = 1;
        xfreq = (int) Math.abs (slope);
      }
      else {
        xfreq = 1;
        yfreq = (int) Math.abs (1.0 / slope);
      }
    }
    schedule.at$createActionTo$message (t, this, checkWorkSelector);
    if (xfreq > 0)
      schedule.at$createActionTo$message (t, this, xmoveSelector);
    if (yfreq > 0)
      schedule.at$createActionTo$message (t, this, ymoveSelector);
  }

  public void incubate () {
    if (incubationRemaining == 0 || (frobbed && !resisting)) {
      if (energy > 0)
        startWork (Globals.env.getCurrentTime () + 1);
      else
        startIncubation (Globals.env.getCurrentTime () + 1);
    } 
    else {
      color = resisting ? ObserverSwarm.MarcusResistColor : ObserverSwarm.MarcusIncubateColor;
      randomWalk ();
      incubationRemaining--;
      schedule.at$createActionTo$message (Globals.env.getCurrentTime () + 1,
                                          this,
                                          incubateSelector);
    }
    clearFrobStatus ();
  }

  public void checkWork () {
    if (energy <= 0) {
      if (xmoveNext != null) {
        ((Action) schedule.remove (xmoveNext)).drop ();
        xmoveNext = null;
      }
      if (ymoveNext != null) {
        ((Action) schedule.remove (ymoveNext)).drop ();
        ymoveNext = null;
      }
      startIncubation (Globals.env.getCurrentTime () + 1);
    }
    else {
      energy -= 5;
      schedule.at$createActionTo$message (Globals.env.getCurrentTime () + 1,
                                          this,
                                          checkWorkSelector);
    }
  }
  
    public void moveAgent(int xo, int yo) {
        super.moveAgent(xo,yo);
        if (!frobbed && !working) energy += 10;
    }

  public void xmove () {
    moveAgent (xoffset, 0);
    xmoveNext = schedule.at$createActionTo$message (Globals.env.getCurrentTime () + xfreq,
                                                    this,
                                                    xmoveSelector);
  }
  public void ymove () {
    moveAgent (0, yoffset);
    ymoveNext = schedule.at$createActionTo$message (Globals.env.getCurrentTime () + yfreq,
                                                    this,
                                                    ymoveSelector);
  }

  public Activity activateIn (Swarm context) {
    super.activateIn (context);

    schedule.activateIn (this);
    return getActivity ();
  }


  public boolean frob (int direction) {
      if (!working) {
          energy -= 10;
          return super.frob (direction);
      } else {
          return false;
      }
  }
}
