import swarm.Globals;

public class Marcus extends Agent {
  Marcus () {
    super ("Marcus");
  }
  public void step () {
    moveAgent (Globals.env.uniformIntRand.getIntegerWithMin$withMax (-5, 5),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-5, 5),
               Globals.env.uniformIntRand.getIntegerWithMin$withMax (-5, 5));
  }
}
