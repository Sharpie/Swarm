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
	// objects and activities.  Note that we now use the Lisp
	// Archiver to create modelSwarm and to load the model's
	// parameters from a file.  The default filename is the
	// name given by the first argument to initSwarm(), above,
	// with the file extension .scm.  In this case that is
	// SimpleBug.scm.
        
	// The second argument to getWithZone$key(), "modelSwarm", is
	// the key in the .scm file which contains the class of the
	// object to be created, ModelSwarm, and the values of the
	// model parameters in the ModelSwarm class that we wish to
	// set when the new object is created: worldXSize, worldYSize,
	// seedProb, bugDensity and endTime.
	modelSwarm = 
	    (ModelSwarm)Globals.env.lispAppArchiver.getWithZone$key(
				 Globals.env.globalZone, "modelSwarm");
	modelSwarm.buildObjects();
	modelSwarm.buildActions();
	modelSwarm.activateIn(null);

	// Now activate the swarm.
	(modelSwarm.getActivity()).run();

	// And drop it when we are finished.
	modelSwarm.drop();
    }
}
