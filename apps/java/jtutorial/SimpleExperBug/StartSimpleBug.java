// StartSimpleBug.java
// Java SimpleBug application. 

import swarm.Globals;
import swarm.defobj.Zone;

public class StartSimpleBug
{
    public static void main (String[] args)
    {
	ExperSwarm experSwarm;

        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm ("SimpleBug", "2.1.1", 
			       "bug-swarm@santafe.edu", args);

	// Create a top-level Swarm object, now experSwarm, build its
	// internal objects and activities and activate it in the null
	// (top-level) context.
        experSwarm = new ExperSwarm(Globals.env.globalZone);
	Globals.env.setWindowGeometryRecordName (experSwarm, "experSwarm");

	experSwarm.buildObjects();
	experSwarm.buildActions();
	experSwarm.activateIn(null);

	// Now start experSwarm and the control panel it provides.
	experSwarm.go();

	// The user has pressed Quit.  Drop everything and return.
	experSwarm.drop();
    }
}
