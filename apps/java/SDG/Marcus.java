import swarm.Globals;

public class Marcus extends Agent {
  Marcus () {
    super ("Marcus");
  }
  public void step () {
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-2, 2));
  }
}
