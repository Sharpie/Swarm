package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;
import swarm.activity.Action;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;
import swarm.Selector;

import swarm.Globals;

import ObserverSwarm;

public class Marcus2d extends Agent2d {
  Schedule schedule;
  Selector incubateSelector;
  Selector checkWorkSelector;
  Selector xmoveSelector, ymoveSelector;
  Action xmoveNext, ymoveNext;
  int xoffset, yoffset;
  int xfreq, yfreq;
  int incubationTime, incubationRemaining;
  boolean working;

  public Marcus2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y, .25, .5, 60, 10);
    
    this.incubationTime = 40;
    schedule = new ScheduleImpl (aZone, true);

    try {
      schedule.at$createActionTo$message
        (0,
         this,
         new Selector (getClass (), "startIncubation", false));
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
  
  public Activity activateIn (Swarm context) {
    super.activateIn (context);

    schedule.activateIn (this);
    return getActivity ();
  }

  public void startIncubation (int t) {
    working = false;
    incubationRemaining = incubationTime;
    resistProbability = sampleResistProbability ();
    energy = sampleEnergy ();
    schedule.at$createActionTo$message (t, this, incubateSelector);
  }

  public void startWork (int t) {
    working = true;
    if (!frobbed)
      direction =
        Globals.env.uniformIntRand.getIntegerWithMin$withMax (0, 359);
    
    if (direction == 90) {
      yfreq = 1;
      xfreq = 0;
      xoffset = 0;
      yoffset = 1;
    } else if (direction == 270) {
      xfreq = 0;
      yfreq = 1;
      xoffset = 0;
      yoffset = -1;
    } else if (direction == 180) {
      xfreq = 1;
      yfreq = 0;
      xoffset = -1;
      yoffset = 0;
    } else if (direction == 0) {
      xfreq = 1;
      yfreq = 0;
      xoffset = 1;
      yoffset = 0;
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
      moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-5, 5),
                 Globals.env.uniformIntRand.getIntegerWithMin$withMax (-5, 5));
      incubationRemaining--;
      schedule.at$createActionTo$message (Globals.env.getCurrentTime () + 1,
                                          this,
                                          incubateSelector);
    }
    resisting = false;
    frobbed = false;
  }

  public void checkWork () {
    if (energy == 0) {
      if (xmoveNext != null) {
        schedule.remove (xmoveNext);
        xmoveNext = null;
      }
      if (ymoveNext != null) {
        schedule.remove (ymoveNext);
        ymoveNext = null;
      }
      startIncubation (Globals.env.getCurrentTime () + 1);
    }
    else {
      energy--;
      schedule.at$createActionTo$message (Globals.env.getCurrentTime () + 1,
                                          this,
                                          checkWorkSelector);
    }
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

  public boolean frob (int direction) {
    if (!working)
      return super.frob (direction);
    else
      return false;
  }

  public Object drawSelfOn (Raster r) {
    if (working)
      r.drawPointX$Y$Color (x, y, ObserverSwarm.MarcusWorkColor);
    else
      r.drawPointX$Y$Color (x, y, resisting ? ObserverSwarm.MarcusResistColor : ObserverSwarm.MarcusIncubateColor);
    return this;
  }
}
