import javax.media.j3d.TransformGroup;

import swarm.Globals;
import swarm.defobj.Zone;

public class SDG extends Organization {
  public SDG (Zone aZone, TransformGroup group) {
    super (aZone, group);

    Agent mgd = new Marcus ();
    Agent gepr = new Glen ();
    
    addAgent (mgd);
    addAgent (gepr);
    
    gepr.moveAgent (0.0, -0.25, 0.25);
    mgd.step ();
  }
}
