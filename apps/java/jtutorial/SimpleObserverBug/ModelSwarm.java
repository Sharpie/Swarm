// ModelSwarm.java
// The top-level ModelSwarm

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;

import swarm.activity.ActionGroupImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;

import swarm.space.Grid2dImpl;
import swarm.collections.ListImpl;

public class ModelSwarm extends SwarmImpl
{
    // Declare the model parameters and their default values.
    public int worldXSize = 80, worldYSize = 80;
    public double seedProb = 0.80;
    public double bugDensity = 0.10;
    public int endTime = 125;

    // Declare some other needed variables.
    public FoodSpace foodSpace;
    public Grid2dImpl bugSpace;
    public ListImpl bugList;
    public ScheduleImpl modelSchedule;

    // This is the constructor for a new ModelSwarm.  All we do is to
    // use the contructor for ModelSwarm's parent class.
    public ModelSwarm(Zone azone)
    {
	// Use the parent class to create a top-level swarm.
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
					 bugSpace, x, y, ++num);
		    bugSpace.putObject$atX$Y(abug, x, y);
		    bugList.addLast(abug);
		    }

	// Report the number of bugs created.
	System.out.println(bugList.getCount() + " bugs were created.");

	// There is no longer any need for a reporter bug as we will
	// be observing the bugs directly through the Observer Swarm.
	// So ...

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

    // This is the method a) for building the list of actions for
    // these objects to accomplish and b) for scheduling these actions
    // in simulated time.
    public Object buildActions()
    {
	Selector sel;
	ActionGroupImpl modelActions;

	// First, use the parent class to initialize the process.
	super.buildActions();

	// Then create an ActionGroup object and insert both a
	// randomWalk message to every bug in the bug list and a
	// checkTime message to modelSwarm. Note that we have
	// routinized the creation of a Selector through a new
	// method. getSelector() is a static method in a new class,
	// SwarmUtils, contained in SwarmUtils.java. The method is
	// overloaded.  It takes either a class name string or an
	// object of the desired class as its first argument, and a
	// message string as the second. We use both forms here.
	modelActions = new ActionGroupImpl(getZone());

	sel = SwarmUtils.getSelector("SimpleBug", "randomWalk");
	modelActions.createActionForEach$message(bugList, sel);

	sel = SwarmUtils.getSelector(this, "checkTime");
	modelActions.createActionTo$message(this, sel);

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
	
    // This is a pretty crude method to end the simulation after an
    // arbitrary number of periods given by the endTime parameter. If
    // the simulation time returned by getCurrentTime() is greater
    // than endTime, we terminate the modelSwarm activity.  The
    // ObserverSwarm will pick this up and handle it.  Note that this
    // is no longer strictly necessary since the user can now stop the
    // simulation at any time using the control panel.  Still, this is
    // a placeholder for a more sophisticated end-of-simulation
    // routine to be introduced later.
    public void checkTime()
    {
	if (Globals.env.getCurrentTime() >= endTime)
	    getActivity().terminate();

        return;
    }
}
