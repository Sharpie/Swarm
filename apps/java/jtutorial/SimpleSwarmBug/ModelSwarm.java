// ModelSwarm.java
// The top-level ModelSwarm.

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;

import swarm.activity.ActionGroupImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;


public class ModelSwarm extends SwarmImpl
{
    // Declare the model parameters and their default values.
    public int worldXSize = 80, worldYSize = 80;
    public double seedProb = 0.20;
    public int endTime = 20000;

    // Declare some other needed variables.
    FoodSpace foodSpace;
    SimpleBug abug;
    ScheduleImpl modelSchedule;

    // This is the constructor for a new ModelSwarm.  All we do is to
    // use the contructor for ModelSwarm's parent class.
    public ModelSwarm(Zone aZone)
    {
	// Use the parent class to create a top-level swarm.
	super(aZone);
    }

    // This is the method for building the model's objects: in this
    // case the food space and the bug.
    public Object buildObjects()
    {
	int xPos, yPos;

	// Use the parent class buildObject() method to initialize the
	// process.
	super.buildObjects();

	// Now create the model's objects.
	// First create the foodspace and seed it with food.
	foodSpace = new FoodSpace(Globals.env.globalZone, 
				  worldXSize, worldYSize);
	foodSpace.seedFoodWithProb(seedProb);

	// Find the middle of the foodspace.
	xPos = (foodSpace.getSizeX())/2;
	yPos = (foodSpace.getSizeY())/2;

	// Create a single SimpleBug, abug, and place it in the
	// foodspace at (xPos, yPos).  The bug knows the size of its
	// world from being passed the pointer to the foodspace in
	// which it is to be created.
	abug = new SimpleBug(Globals.env.globalZone, foodSpace, 
			     xPos, yPos, 1);

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
	// randomWalk message to abug.  Since the
	// createActionTo$message() method requires a selector ojbect to
	// "contain" the method, and since the creation of a selector
	// can throw an exception, that process is included in a
	// try/catch block. The first argument, abug.getClass(),
	// retrieves the class to which abug belongs while the second
	// argument, "randomWalk", is the name of the particular abug
	// method we wish to add to the list of actions.  Note that
	// randomWalk must have been declared public.
	modelActions = new ActionGroupImpl(getZone());
	try
	    {
	    sel = new Selector(abug.getClass(), "randomWalk", false);
	    modelActions.createActionTo$message(abug, sel);
	    } catch (Exception e)
		{
		System.err.println("Exception randomWalk: " + 
				   e.getMessage ());
		System.exit(1);
		}

	// Next we add to modelActions another message, this one to
	// modelSwarm itself telling it to check the simulation
	// time. We use this to send a timestamp to the console and to
	// stop the model after a specified number of periods.
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
