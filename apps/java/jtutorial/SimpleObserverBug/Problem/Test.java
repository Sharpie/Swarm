
import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;
import swarm.defobj.ZoneImpl;
import swarm.simtoolsgui.GUISwarm;
import swarm.simtoolsgui.GUISwarmImpl;

import swarm.activity.ActionGroup;
import swarm.activity.ActionGroupImpl;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;



public class Test
{
    public static void main (String[] args)
    {
	ObserverSwarm displaySwarm;

        // Swarm initialization: all Swarm apps must call this first.
        Globals.env.initSwarm ("SimpleBug", "2.1", 
			       "bug-swarm@santafe.edu", args);

        displaySwarm = new ObserverSwarm(Globals.env.globalZone);

	displaySwarm.buildObjects();
	displaySwarm.buildActions();
	displaySwarm.activateIn(null);

	displaySwarm.go();

	displaySwarm.drop();
    }
}

class ObserverSwarm extends GUISwarmImpl
{
    ZoneImpl modelZone;
    ModelSwarm modelSwarm;
    ScheduleImpl displaySchedule;

    boolean simulationFinished = false;

    public ObserverSwarm(Zone azone)
    {
	super(azone);
    }

    public Object buildObjects()
    {
	super.buildObjects();

	modelZone = new ZoneImpl(getZone());
	modelSwarm = new ModelSwarm(modelZone);

	getControlPanel().setStateStopped();
	System.out.println("Now building objects.");

	modelSwarm.buildObjects();
	modelSwarm.setDisplaySwarm(this);

	return this;
    }

    public Object buildActions()
    {
	Selector sel;
	ActionGroupImpl displayActions;

	super.buildActions();

	modelSwarm.buildActions();

	displayActions = new ActionGroupImpl(getZone());
	try
	    {
	    sel = new Selector(getClass(), "checkForDone", false);
	    displayActions.createActionTo$message(this, sel);
	    }
	catch (Exception e)
	    {
	    System.out.println("Selector error.");
	    System.exit(1);
	    }

	displaySchedule = new ScheduleImpl(getZone(), 1);
	displaySchedule.at$createAction(0, displayActions);

	return this;
    }

    public Activity activateIn(Swarm swarmContext)
    {
	super.activateIn(swarmContext);

	modelSwarm.activateIn(this);
	displaySchedule.activateIn(this);

	// System.out.println("displaySwarm activated: " + getActivity());
	return getActivity();
    }

    public void checkForDone()
    {
	System.out.println("checkForDone " +
			   Globals.env.getCurrentTime());
	System.out.println("modelSwarm's status is " 
			   + modelSwarm.getActivity().getStatus().getName());

	if (simulationFinished)
    // if (modelSwarm.getActivity() == null)
	    {
	    System.out.println("I said to QUIT!");
	    getControlPanel().setStateQuit();
	    }
	else if (modelSwarm.getActivity().getStatus().getName().equals("Completed")
	    || modelSwarm.getActivity().getStatus().getName().equals("Terminated"))
	    // else if (simulationFinished)
	    {
	    System.out.println("simulationFinished "
			       + Globals.env.getCurrentTime());
	    // (modelSwarm.getActivity()).terminate();
	    // (modelSwarm.getActivity()).drop();
	    simulationFinished = true;

	    System.out.println("Press QUIT when ready.");
	    getControlPanel().setStateStopped();
	    }
    }

    // Receive a message that the simulation is done and set
    // simulationFinished to true.
    public void simulationDone()
    {
	simulationFinished = true;
    }
}

class ModelSwarm extends SwarmImpl
{
    ScheduleImpl modelSchedule;
    ObserverSwarm displaySwarm;

    public ModelSwarm(Zone azone)
    {
	super(azone);
    }

    public void setDisplaySwarm(ObserverSwarm dS)
    {
	displaySwarm = dS;
    }

    public Object buildObjects()
    {
	super.buildObjects();

	return this;
    }

    public Object buildActions()
    {
	Selector sel;
	ActionGroupImpl modelActions;

	modelActions = new ActionGroupImpl(getZone());
	try
	    {
	    sel = new Selector(getClass(), "checkTime", false);
	    modelActions.createActionTo$message(this, sel);
	    }
	catch (Exception e)
	    {
	    System.out.println("Selector error.");
	    System.exit(1);
	    }

	modelSchedule = new ScheduleImpl(getZone(), 1);
	modelSchedule.at$createAction(0, modelActions);

	return this;
    }

    public Activity activateIn(Swarm swarmContext)
    {
	super.activateIn(swarmContext);

	modelSchedule.activateIn(this);

	// System.out.println("modelSwarm activated: " + getActivity());
	return getActivity();
    }

    public void checkTime()
    {
	int time;

	time = Globals.env.getCurrentTime();
	System.out.println("checkTime " + time);
	if (time >= 5)
	    {
		//displaySwarm.simulationDone();
		this.getActivity().terminate();
	    }

	return;
    }
}
