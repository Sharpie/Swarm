import swarm.objectbase.SwarmImpl;
import swarm.defobj.Zone;

public class Organization extends SwarmImpl {
  Context context;

  public Organization (Zone aZone, Context context) {
    super (aZone);
    this.context = context;
  }

  public void addAgent (Agent3d agent) {
    context.addAgent (agent);
  }
}
