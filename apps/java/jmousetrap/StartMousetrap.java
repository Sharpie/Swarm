// Java mousetrap application. Copyright © 1999 Santa Fe Institute.
// This application is distributed without any warranty; without even
// the implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.Globals;
import swarm.defobj.ZoneImpl;
/**
 * This class is the `main' function of the Mousetrap application */
public class StartMousetrap {
  public static void main (String[] args) {
    Globals.env.initSwarm(args);
    
    if (Globals.env.guiFlag) {
      MousetrapObserverSwarm topLevelSwarm =
        new MousetrapObserverSwarm (Globals.env.globalZone);
        
      Globals.env.setWindowGeometryRecordName (topLevelSwarm);
      if (topLevelSwarm.buildObjects () != null) {
        topLevelSwarm.buildActions ();
        topLevelSwarm.activateIn (null);
        topLevelSwarm.go ();
      }
      topLevelSwarm.drop ();
    }
    else {
      MousetrapBatchSwarm topLevelSwarm =
        new MousetrapBatchSwarm (Globals.env.globalZone);
      topLevelSwarm.buildObjects ();
      topLevelSwarm.buildActions ();
      topLevelSwarm.activateIn (null);
      topLevelSwarm.go ();
      topLevelSwarm.drop ();
    }
  }
}
