import javax.media.j3d.TransformGroup;
import swarm.activity.Activity;
import swarm.objectbase.Swarm;
import swarm.Globals;
import swarm.defobj.Zone;

public class SDG extends Organization {
  Agent3d mgd, gepr;

  public SDG (Zone aZone, Context context) {
    super (aZone, context);

    mgd = new Marcus (aZone);
    gepr = new Glen (aZone);
    
    addAgent (mgd);
    addAgent (gepr);
  }

  public Activity activateIn (Swarm swarmContext) {
    super.activateIn (swarmContext);

    System.out.println ("Activating..");
    mgd.activateIn (this);
    gepr.activateIn (this);
    return getActivity ();
  }
}
