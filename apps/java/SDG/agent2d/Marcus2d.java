package agent2d;

import swarm.objectbase.SwarmImpl;
import swarm.objectbase.Swarm;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;
import swarm.defobj.Zone;
import swarm.space.Grid2d;
import swarm.gui.Raster;
import swarm.Selector;

import swarm.Globals;

public class Marcus2d extends Agent2d {
  Schedule schedule;

  public Marcus2d (Zone aZone, Grid2d world, int x, int y) {
    super (aZone, world, x, y);

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
    int newx, newy;
    world.putObject$atX$Y (null, x, y);
    newx = x;
    newy = y;
    newx += Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2);
    newy += Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2);
    newx = Math.abs (newx) % world.getSizeX ();
    newy = Math.abs (newy) % world.getSizeY ();
    System.out.println ("moving from " + x + "," + y + " to " + newx + "," + newy);
    x = newx;
    y = newy;
    world.putObject$atX$Y (this, x, y);
  }

  public Object drawSelfOn (Raster r) {
    System.out.println ("drawing " + x + "," + y);
    r.drawPointX$Y$Color (x, y, (byte) 0);
    return this;
  }
}
