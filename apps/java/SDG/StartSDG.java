import swarm.Globals;

public class StartSDG {
  public StartSDG () {
    Organization sdg = new SDG (Globals.env.globalZone);
  }

  public static void main (String[] args) {
    Globals.env.initSwarm ("SDG", "0.0", "bug-swarm@swarm.org", args);
    new StartSDG ();
  }
}
