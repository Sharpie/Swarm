// StartSimpleBug.java
// Java SimpleBug application. 

import swarm.Globals;
import swarm.defobj.Zone;

public class StartSimpleBug
{
    public static void main (String[] args)
    {
	ModelSwarm modelSwarm;

        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm ("SimpleBug", "2.1", 
			       "bug-swarm@santafe.edu", args);

	// Create a top-level Swarm object and build its internal
	// objects and activities.
	modelSwarm = new ModelSwarm(Globals.env.globalZone);
	modelSwarm.buildObjects();
	modelSwarm.buildActions();
	modelSwarm.activateIn(null);

	// Now activate the swarm.
	(modelSwarm.getActivity()).run();

	// And drop it when we are finished.
	modelSwarm.drop();
    }
}
