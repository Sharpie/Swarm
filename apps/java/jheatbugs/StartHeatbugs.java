// Java Heatbugs application. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file COPYING for details and terms of copying.

import swarm.Globals;

public class StartHeatbugs {
    /** The main() function is the top-level place where everything
        starts.  For a typical Swarm simulation, in main() you create
        a toplevel Swarm, let it build and activate, and set it to
        running.  */
    public static void main (String[] args)
    {
        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm (args);
        
        // swarmGUIMode is set in initSwarm(). It's set to be `false'
        // if you typed `heatbugs --batchmode' or `heatbugs
        // -b'. Otherwise, it's set to `true'.

        if (Globals.env.guiFlag) {
            // We've got graphics, so make a full ObserverSwarm to get
            // GUI objects
            HeatbugObserverSwarm theTopLevelSwarm = 
                new HeatbugObserverSwarm (Globals.env.globalZone);
            Globals.env.setWindowGeometryRecordName (theTopLevelSwarm);
            
            theTopLevelSwarm.buildObjects ();
            theTopLevelSwarm.buildActions ();
            theTopLevelSwarm.activateIn (null);
            theTopLevelSwarm.go ();
        }
        else {
            /* call batchSwarm here when implemented */
            System.out.println ("Batch Swarm not implemented yet!");
        }
    }
}
