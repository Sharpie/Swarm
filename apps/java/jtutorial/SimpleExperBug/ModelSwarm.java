// ModelSwarm.java
// The top-level ModelSwarm

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.objectbase.EmptyProbeMap;
import swarm.objectbase.EmptyProbeMapImpl;

import swarm.activity.ActionGroupImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;

import swarm.space.Grid2dImpl;
import swarm.collections.ListImpl;

public class ModelSwarm extends SwarmImpl
{
    // Declare the model parameters and their default values. )The
    // ParameterManager sill override these settings.)
    public int worldXSize = 80, worldYSize = 80;
    public double seedProb = 0.80;
    public double bugDensity = 0.10;
    public int bugHardiness = 10;

    // We probe these too.
    public int numberOfBugs = 0;
    public int currentNumberOfBugs = 0;
    public int simulatedTime = 0;

    // Declare some other needed variables.
    public FoodSpace foodSpace;
    public Grid2dImpl bugSpace;
    public ListImpl bugList, reaperQueue;
    public ScheduleImpl modelSchedule;

    // This is the constructor for a new ModelSwarm.
    public ModelSwarm(Zone azone)
    {
	// Use the parent class to create a Swarm.
	super(azone);
    }

    // This is the method for building the model's objects: the food
    // space, the two-dimensional positioning grid, and the host of
    // bugs.
    public Object buildObjects()
    {
	int x, y, num;
	SimpleBug abug;

	// use the parent class buildObject() method to initialize the
	// process
	super.buildObjects();

	// Now create the model's objects.
	// First create the foodspace and seed it with food.
	foodSpace = new FoodSpace(Globals.env.globalZone, 
				  worldXSize, worldYSize);
	foodSpace.seedFoodWithProb( seedProb );

	// Then create a 2-D grid that will be used to keep track of
	// each bug's position, insuring that no two bugs will ever be
	// on the same cell. Initialize the grid to be empty.
	bugSpace = new Grid2dImpl(Globals.env.globalZone, 
				   worldXSize, worldYSize);
	bugSpace.fastFillWithObject(null);

	// Now create a List object to manage all the bugs we are
	// about to create.
	bugList = new ListImpl(Globals.env.globalZone);

	// Iterate over the grid with a certain probability of
	// creating a bug at each site.  If a bug is created, put it
	// on the grid and add it to the end of the bug list.  Note
	// that we increment the bug number, num, each time a bug is
	// created.
	num = 0;
	for (y = 0; y < worldYSize; y++)
	    for (x = 0; x < worldXSize; x++)
		if ( Globals.env.uniformDblRand.getDoubleWithMin$withMax(
						  0.0, 1.0) <= bugDensity)
		    {
		    abug = new SimpleBug(Globals.env.globalZone, foodSpace, 
					 bugSpace, x, y, ++num, 
					 bugHardiness, this);
		    bugSpace.putObject$atX$Y(abug, x, y);
		    bugList.addLast(abug);
		    }

	// Report the number of bugs created.
	numberOfBugs = bugList.getCount();
	currentNumberOfBugs = numberOfBugs;

	// Finally, create a list for holding those bugs that have
	// died and are marked for removal.
	reaperQueue = new ListImpl(Globals.env.globalZone);

	// We're done.
	return this;
    }

    // The next three methods return some useful information about the
    // created ModelSwarm to the caller.  They will be used by the
    // Observer Swarm.
    public ListImpl getBugList()
    {
	return bugList;
    }

    public Grid2dImpl getWorld()
    {
	return bugSpace;
    }

    public FoodSpace getFood()
    {
	return foodSpace;
    }

    // This method is used to set the model's parameters for each run.
    public void setParameters(int wXSize, int wYSize, double sProb, 
				double bDensity, int bHardiness)
    {
	worldXSize = wXSize;
	worldYSize = wYSize;
	seedProb = sProb;
	bugDensity = bDensity;
	bugHardiness = bHardiness;
    }

    // This method reports on the simulation time.
    public int getTime()
    {
	return getActivity().getCurrentTime();
    }

    // This method reports on the initial number of bugs.
    public int getBugs()
    {
	return numberOfBugs;
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

	// Then create an ActionGroup object and insert a randomWalk
	// message to every bug in the bug list.  We've taken out the
	// old "checkTime" message as we have a new way to end the
	// simulation.
	modelActions = new ActionGroupImpl(getZone());

	sel = SwarmUtils.getSelector("SimpleBug", "randomWalk");
	modelActions.createActionForEach$message(bugList, sel);

	// We've added another action here, the cleaning out of the
	// reaperQueue of bugs that have died.  It will be done only
	// after all the bugs have taken their random walks.
	sel = SwarmUtils.getSelector(this, "reapBugs");
	modelActions.createActionTo$message(this, sel);

	// And we finally update the simulated time and the probe
	// displays.
	sel = SwarmUtils.getSelector(this, "updateSimulatedTime");
	modelActions.createActionTo$message(this, sel);
	sel = SwarmUtils.getSelector(Globals.env.probeDisplayManager,
				     "update");
	modelActions.createActionTo$message(Globals.env.probeDisplayManager,
					    sel);

	// Now create the schedule and set the repeat interval to unity.
	modelSchedule = new ScheduleImpl(getZone(), 1);

	// Finally, insert the action list into the schedule at period zero
	modelSchedule.at$createAction(0, modelActions);

	return this;
    }

    // This method specifies the context in which the model is to be run.
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

    // This simply keeps the simulatedTime variable up to date.
    public void updateSimulatedTime()
    {
	simulatedTime = getActivity().getCurrentTime();
    }

    // bugDeath insures the orderly death of a bug.  First we check to
    // see that the agent is on the grid where it thinks it is.  If
    // so, we remove it from the grid and add it to reaperQueue for
    // later removal.
    public void bugDeath(SimpleBug abug)
    {
	if ((SimpleBug)bugSpace.getObjectAtX$Y(abug.xPos, abug.yPos) == abug)
	    {
	    bugSpace.putObject$atX$Y(null, abug.xPos, abug.yPos);
	    reaperQueue.addLast(abug);
	    }
    }

    // reapBugs drops the bugs queued for removal.  It is scheduled
    // for action when it is safe to do so, i.e., after all the bugs
    // have finished their random walks.  We get each agent in the
    // reaperQueue and remove it from the bugListf.  If we run out of
    // bugs, we terminate the simulation.
    public void reapBugs()
    {
	SimpleBug abug;

	while (reaperQueue.getCount() != 0)
	    {
	    abug = (SimpleBug)reaperQueue.removeFirst();
	    bugList.remove(abug);
	    // System.out.println("Bug " + abug.bugNumber + 
	    //	       " has died of hunger.");
	    abug.drop();
	    currentNumberOfBugs--;
	    }

	// Check the number of remaining bugs and quit the simulation
	// if there are none left, by terminating the ModelSwarm
	// activity.
	if (bugList.getCount() == 0)
	    getActivity().terminate();

    }
}
