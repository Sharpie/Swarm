// StartSimpleBug.java
// Java SimpleBug application. 

import swarm.Globals;
import swarm.defobj.Zone;

public class StartSimpleBug
{
    public static void main (String[] args)
    {
	ObserverSwarm displaySwarm;

        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm ("SimpleBug", "2.1", 
			       "bug-swarm@santafe.edu", args);

	// Create a top-level Swarm object, now DisplaySwarm, and
	// build its internal objects and activities. Note that we use
	// the Lisp Archiver to create displaySwarm and to load the
	// model's display parameters from the a file.  The default
	// filename is the name given by the first argument to
	// initSwarm, above, with the file extension .scm.  In this
	// case that is SimpleBug.scm.
        
        // "displaySwarm", the second argument to getWithZone$key(), is
        // the key in the .scm file which contains the values of the
        // display parameters in the ObserverSwarm class.  For now
        // there is only one parameter, displayFrequency.

        displaySwarm = 
	    (ObserverSwarm)Globals.env.lispAppArchiver.getWithZone$key(
				 Globals.env.globalZone, "displaySwarm");

	displaySwarm.buildObjects();
	displaySwarm.buildActions();
	displaySwarm.activateIn(null);

	// Now start the displaySwarm and the control panel it
	// provides.
	displaySwarm.go();

	// The user has pressed Quit.  Drop everything and return.
	displaySwarm.drop();
    }
}
