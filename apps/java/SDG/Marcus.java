import swarm.Globals;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;
import swarm.objectbase.Swarm;
import swarm.defobj.Zone;
import swarm.Selector;

public class Marcus extends Agent3d {
  public Marcus (Zone aZone) {
    super (aZone, "Marcus");
  }

  public Activity activateIn (Swarm swarmContext) {
    super.activateIn (swarmContext);
    
    return getActivity ();
  }
  
  public void stepAgent () {
    System.out.println ("here");
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2));
  }
}
