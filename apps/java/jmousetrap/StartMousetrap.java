import swarm.Globals;
import swarm.defobj.ZoneImpl;
/**
 * This class is the `main' function of the Mousetrap application */
public class StartMousetrap
{
  public static void nag (String s)
  {
    System.out.println ( s);
  }
  public static void main (String[] args)
  {
    Globals.env.initSwarm(args);

    if (Globals.env.guiFlag) {
        
      MousetrapObserverSwarm topLevelSwarm 
        = new MousetrapObserverSwarm(Globals.env.globalZone);
        
      Globals.env.setWindowGeometryRecordName (topLevelSwarm);
      topLevelSwarm.buildObjects ();
      topLevelSwarm.buildActions ();
      topLevelSwarm.activateIn (null);
      topLevelSwarm.go ();
        
    }
    else {
      MousetrapBatchSwarm topLevelSwarm 
        = new MousetrapBatchSwarm((ZoneImpl)Globals.env.globalZone);
      topLevelSwarm.buildObjects();
      topLevelSwarm.buildActions();
      topLevelSwarm.activateIn(null);
      topLevelSwarm.go();
    }
  }
}
