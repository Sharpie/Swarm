import com.sun.j3d.utils.applet.MainFrame;
import javax.media.j3d.BranchGroup;
import swarm.Globals;

public class SDG extends Organization {
  public BranchGroup createSceneGraph () {
    BranchGroup root = super.createSceneGraph ();
    Agent mgd = new Marcus ();
    Agent gepr = new Glen ();

    addAgent (mgd);
    addAgent (gepr);
    
    gepr.moveAgent (0.0, -0.25, 0.25);
    mgd.step ();

    return root;
  }

  public SDG (String[] args) {
    super (args);
  }

  public static void main (String[] args) {
    Globals.env.initSwarm ("SDG", "0.0", "bug-swarm@swarm.org", args);
    new MainFrame (new SDG (args), 200, 200);
  }
}
