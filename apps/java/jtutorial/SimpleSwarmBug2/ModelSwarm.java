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
    public SimpleBug reportBug;

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

	// Finally, enlist a "reporter" bug to let us know how things
	// are going.  We just pop the first bug off the list, record
	// its id, and return it to the list.
	reportBug = (SimpleBug)bugList.removeFirst();
	bugList.addFirst(reportBug);
	System.out.println("The lucky reporter bug is bug number " + 
			   reportBug.bugNumber + ".");

	// We're done.
	return this;
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

	// Then create an ActionGroup object and tell it to send a
	// randomWalk message to every bug in the bug list. Note the
	// change here from using the method createActionTo$message()
	// to send a message to a single object, to using the method
	// createActionForEach$message() to send the same message to
	// all the objects in a list.
	modelActions = new ActionGroupImpl(getZone());
	try
	    {
	    // Note the use here of the forName() method of the
	    // "Class" class to pass the first argument to the
	    // creation of the selector. The first argument to the
	    // creation of a selector requires an object of type Class
	    // that identifies the class of the object to which the
	    // message should be sent, in this case a SimpleBug. If we
	    // had an instance of a SimpleBug available, say abug,
	    // that first argument could use the getClass() method
	    // derived for all classes from the superclass, Object.
	    // The argument would be "abug.getClass()".  Indeed, we
	    // can use getClass() when we create below the selector
	    // for the reportIfEaten message to the reportBug since
	    // reportBug is a created object.  (Of course, reportBug
	    // is an instance of a SimpleBug and we could use
	    // reportBug.getClass() instead of
	    // Class.forName("SimpleBug") in creating our randomWalk
	    // selector, but an instance such as reportBug might not
	    // always be available and it's good to know that we don't
	    // really need one.)
	    sel = new Selector(Class.forName("SimpleBug"), 
			       "randomWalk", false);
	    modelActions.createActionForEach$message(bugList, sel);
	    } catch (Exception e)
		{
		System.err.println("Exception randomWalk: " + 
				   e.getMessage ());
		System.exit(1);
		}

	// Next we will create a message to the report bug to tell us
	// what it is doing, and add that message to the ActionGroup.
	try
	    {
	    sel = new Selector(reportBug.getClass(), 
			       "reportIfEaten", false);
	    modelActions.createActionTo$message(reportBug, sel);
	    } catch (Exception e)
		{
		System.err.println("Exception reportIfEaten: " + 
				   e.getMessage ());
		System.exit(1);
		}

	// Our last addition to the ActionGroup is the message to
	// modelSwarm itself to check the simulation time. We use this
	// to send a timestamp to the console and to stop the model
	// after a specified number of periods.
	try
	    {
	    sel = new Selector(this.getClass(), "checkTime", false);
	    modelActions.createActionTo$message(this, sel);
	    } catch (Exception e)
		{
		System.err.println("Exception checkTime " + e.getMessage ());
		System.exit(1);
		}

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
    // than endTime, we send a message to the console and terminate
    // the current activity.  We also use this method to send a
    // timestamp to the console.
    public void checkTime()
    {
	int i, t;
	int interval = 5;

	if (Globals.env.getCurrentTime() >= endTime)
	    {
	    // Terminate the simulation.
	    System.out.println("We've reached our endTime at period " + 
			       Globals.env.getCurrentTime());
	    Globals.env.getCurrentSwarmActivity().terminate();
	    }
	else
	    {
		for (i = 0; (t = (int)Math.pow(interval, (double)i)) 
			                               <= endTime; i++)
		    if (t == Globals.env.getCurrentTime())
			System.out.println("The time is " + t);
	    }

        return;
    }
}
