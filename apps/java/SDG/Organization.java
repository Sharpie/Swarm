import swarm.objectbase.SwarmImpl;
import swarm.defobj.Zone;

import javax.media.j3d.TransformGroup;

public class Organization extends SwarmImpl {
  TransformGroup group;

  public Organization (Zone aZone, TransformGroup group) {
    super (aZone);
    this.group = group;
  }

  public void addAgent (Agent agent) {
    group.addChild (agent.getTransformGroup ());
  }
}
