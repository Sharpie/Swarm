// The ModelSwarm

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;
import swarm.space.Grid2dImpl;
import swarm.collections.ListImpl;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.objectbase.EmptyProbeMap;
import swarm.objectbase.EmptyProbeMapImpl;
import swarm.objectbase.VarProbe;
import swarm.objectbase.VarProbeImpl;
import swarm.objectbase.VarProbeC;
import swarm.objectbase.VarProbeCImpl;

import swarm.activity.ActionGroupImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;


public class ModelSwarm extends SwarmImpl
{
    // declare the model parameters and their default values.
    public int worldXSize = 80, worldYSize = 80;
    public int stepSize = 1;
    public double seedProb = 0.5, bugDensity = 0.1;

    // declare other needed variables
    public FoodSpace foodSpace;
    public Grid2dImpl gridSpace;
    public ListImpl bugList;
    public EmptyProbeMapImpl probeMap;

    public ScheduleImpl modelSchedule;

    // This is the constructor for a new ModelSwarm.
    public ModelSwarm(Zone azone)
    {
	// Use the parent class (SwarmImpl) constructor to create a
	// swarm.
	super(azone);

	// Build a customized probe map.  Without a probe map
	// the default is to show all variables and messages.  Here we
	// choose to customize the appearance of the probe, giving a
	// nicer interface.

	// Create the probe map and give it the ModelSwarm class.
	probeMap = new EmptyProbeMapImpl(azone, getClass());

	// Now add probes for the variables we wish to probe.
	probeMap.addProbe(getProbeForVariable("worldXSize"));
	probeMap.addProbe(getProbeForVariable("worldYSize"));
	probeMap.addProbe(getProbeForVariable("seedProb"));
	probeMap.addProbe(getProbeForVariable("bugDensity"));
	probeMap.addProbe(getProbeForVariable("stepSize"));

	// And finaly install our probe map into the probeLibrary.
	// Note that this library was created by initSwarm.
	Globals.env.probeLibrary.setProbeMap$For(probeMap, getClass());
    }

    // This is the method for building the model's objects: the food
    // space, the two-dimensional positioning grid, and the host of
    // bugs.
    public Object buildObjects()
    {
	int x, y;
	Simplebug abug;

	// use the parent class to initialize the process
	super.buildObjects();

	// Now create the model's objects.
	// First create the foodspace and seed it with food.
	foodSpace = new FoodSpace(Globals.env.globalZone, worldXSize, worldYSize);
	foodSpace.seedFoodWithProb( seedProb );

	// Then create a 2-d grid that will be used to keep track of
	// each bug's position, insuring that no two bugs will ever be
	// on the same cell. Initialize the grid to be empty.
	gridSpace = new Grid2dImpl(Globals.env.globalZone, worldXSize, worldYSize);
	gridSpace.fastFillWithObject(null);

	// Now create a List object to manage all the bugs we are
	// about to create.
	bugList = new ListImpl(Globals.env.globalZone);

	// Iterate over the grid with a certain probability of
	// creating a bug at each site.  If a bug is created, put it
	// on the grid and in the bug list.
	for (y = 0; y < worldYSize; y++)
	    for (x = 0; x < worldXSize; x++)
		if ( Globals.env.uniformDblRand.getDoubleWithMin$withMax(0.0, 1.0) 
		     <= bugDensity)
		    {
		    abug = new Simplebug(foodSpace, gridSpace, x, y, stepSize);
		    gridSpace.putObject$atX$Y(abug, x, y);
		    bugList.addLast(abug);
		    }

	// There is no longer any need for a reporter bug as we will
	// be observing the bugs directly through the Observer Swarm.

	// We're done.
	return this;
    }

    // The next three methods return some useful information about the
    // created ModelSwarm.  They will be used by the Observer Swarm.

    public ListImpl getBugList()
    {
	return bugList;
    }

    public Grid2dImpl getWorld()
    {
	return gridSpace;
    }

    public FoodSpace getFood()
    {
	return foodSpace;
    }

    // This is the method a) for building the list of actions for
    // these objects to accomplish and b) for scheduling these actions
    // in simulated time.
    public Object buildActions()
    {
	Selector sel;
	ActionGroupImpl modelActions;

	// First, use the parent class to initialize the process.
	super.buildActions();

	// Now create an ActionGroup object and tell it to send a
	// randomWalk message to every bug in the bug list.  Note that
	// the method must have been declared public.

	modelActions = new ActionGroupImpl(getZone());
	sel = SwarmUtils.getSelector("Simplebug", "randomWalk");
	modelActions.createActionForEach$message(bugList, sel);

	// Now create the schedule and set the repeat interval to unity.
	modelSchedule = new ScheduleImpl(getZone(), 1);

	// finally, insert the action list into the schedule at period zero
	modelSchedule.at$createAction(0, modelActions);

	return this;
    }

    // This method specifies the context (space) in which this swarm
    // is to be run.
    public Activity activateIn(Swarm swarmContext)
    {
	// Use the parent class to activate ourselves in the context
	// passed to us.
	super.activateIn(swarmContext);

	// Then activate the schedule in ourselves.
	modelSchedule.activateIn(this);

	// Finally, return the activity we have built.
	return getActivity();
    }

	
}
