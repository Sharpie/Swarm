// ObserverSwarm.java The observer swarm is collection of objects that
// are used to run and observe the ModelSwarm that actually comprises
// the simulation.

import swarm.Globals;
import swarm.Selector;

import swarm.defobj.Zone;
import swarm.defobj.ZoneImpl;
import swarm.defobj.Symbol;

import swarm.gui.Colormap;
import swarm.gui.ColormapImpl;
import swarm.gui.ZoomRaster;
import swarm.gui.ZoomRasterImpl;

import swarm.space.Value2dDisplay;
import swarm.space.Value2dDisplayImpl;
import swarm.space.Object2dDisplay;
import swarm.space.Object2dDisplay;
import swarm.space.Object2dDisplayImpl;

import swarm.simtoolsgui.GUISwarm;
import swarm.simtoolsgui.GUISwarmImpl;

import swarm.activity.ActionGroup;
import swarm.activity.ActionGroupImpl;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.objectbase.EmptyProbeMap;
import swarm.objectbase.EmptyProbeMapImpl;

import swarm.collections.ListImpl;

// ObserverSwarm is a subclass of GUISwarm implementation class.
public class ObserverSwarm extends GUISwarmImpl
{
    // Declare the display parameters and their default values.
    public int displayFrequency = 1;
    public int zoomFactor = 8;
    public int simulatedTime = 0;
    public int numberOfBugs = 0;

    // A flag to signal the end of the simulation.
    public boolean simulationFinished = false;

    // Declare other variables local to ObserverSwarm.
    ModelSwarm modelSwarm;
    ZoomRaster worldRaster;
    Value2dDisplay foodDisplay;
    Object2dDisplay bugDisplay;
    ScheduleImpl displaySchedule;

    // This is the constructor for a new ObserverSwarm.
    public ObserverSwarm(Zone azone)
    {
	// Use the parent class to create an observer swarm.
	super(azone);

	// Build a custom probe map.  Without a probe map, the default
	// is to show all variables and messages.  Here we choose to
	// customize the appearance of the probe, giving a nicer
	// interface.

	// Create the probe map and give it the ObserverSwarm class.
	EmptyProbeMapImpl probeMap = 
	    new EmptyProbeMapImpl(azone, getClass());

	// Now add probes for the variables we wish to probe, using
	// the method in our SwarmUtils class.
	probeMap.addProbe(getProbeForVariable("displayFrequency"));
	probeMap.addProbe(getProbeForVariable("zoomFactor"));
	probeMap.addProbe(getProbeForVariable("simulatedTime"));

	// And finaly install our probe map into the probeLibrary.
	// Note that this library object was automatically created by
	// initSwarm.
	Globals.env.probeLibrary.setProbeMap$For(probeMap, getClass());
    }

    // The buildObjects method.
    public Object buildObjects()
    {
	Zone modelZone;
	Colormap colormap;
	Selector sel;

	// Use the parent class to initialize the process.
	super.buildObjects();

	// First we create the model that we're actually observing, by
	// creating an instance of the ModelSwarm class, modelSwarm.
	// modelSwarm will now be a subSwarm of this top-level
	// ObserverSwarm rather that the top-level Swarm in its own
	// right.  We create modelSwarm in its own newly-created Zone
	// so that modelSwarm's storage is segregated from the rest of
	// the application.  Note that as in SimpleSwarmBug3, we are
	// reading the modelSwarm parameters from a file and so use
	// the List Archiver to create modelSwarm.

        modelZone = new ZoneImpl(getZone());
	modelSwarm = 
	    (ModelSwarm)Globals.env.lispAppArchiver.getWithZone$key(
				 modelZone, "modelSwarm");

	// Now create probe objects on the model and on ourselves.
	// This provides a simple interface to allow the user to
	// change the model and simulation parameters.  Note that the
	// createArchivedProbeDisplay() method is provided by the Swarm
	// environment, set up by the call to initSwarm().
	Globals.env.createArchivedProbeDisplay(modelSwarm, "modelSwarm");
	Globals.env.createArchivedProbeDisplay(this, "observerSwarm");


	// Instruct the control panel to wait for a button event: we
	// halt here until someone hits a control panel button.
	// Eventually this will allow the user a chance to fill in
	// parameters before the simulation runs.
	getControlPanel().setStateStopped();
        
	// OK - the user has pressed a button.  Now we're ready to
	// start.

	// But first, there are two items on the observer swarm's
	// probe display the user is not allowed to change.  Doing so
	// will have no effect since the probed variables are actually
	// "copies" of the real variables in Global.env and the model
	// swarm and these copies are constantly updated from those
	// real values. However it's nice to tell the user that her
	// attempt is for naught.
	if (simulatedTime != 0 || numberOfBugs != 0)
	    {
	    System.out.println(
	        "The user is not allowed to change the simulated time\n"
		+ "or the number of bugs.  They are determined internally\n"
		+ "to the model.");
	    }

	// Allow the model swarm to build its objects.
 	modelSwarm.buildObjects();

	// Now build the GUI display objects.

	// First, create a colormap, the correspondence between a
	// color and a byte integer code.  This is a global resource
	// which is used by lots of different objects. Then set the
	// three colors we will be using.  Since the FoodSpace grid
	// uses one to indicate a cell with food and zero to indicate
	// a cell without food, FoodSpace cells will be displayed in
	// red or black depending on whether they contain food or not.
	// We'll use green to indicate the location of our bugs
	// (yellow for a hungry bug.)
	colormap = new ColormapImpl(getZone());
	colormap.setColor$ToName((byte)0, "black");
	colormap.setColor$ToName((byte)1, "red");
	colormap.setColor$ToName((byte)2, "green");
	colormap.setColor$ToName((byte)3, "yellow");


	// Now tell each of the bugs in the model to set its default
	// display color to green (2).  We do this by getting the list
	// of bugs created in modelSwarm and interating through it.
	ListImpl bugList = modelSwarm.getBugList();
	for (int i = 0; i < bugList.getCount(); i++)
	    {
	    SimpleBug bug = (SimpleBug)bugList.atOffset(i);
	    bug.setBugColor((byte)2);
	    } 

	// Next, create a "raster widget", a 2-dimensional display
	// window.  We tell the raster what to do if it dies, give it
	// its colormap, set its zoom factor (its actual size on the
	// display screen), set its virtual dimensions to the size of
	// our world, and give it its title.
	worldRaster = new ZoomRasterImpl (getZone(), "worldRaster");
	sel = SwarmUtils.getSelector(this, "_worldRasterDeath_");
	worldRaster.enableDestroyNotification$notificationMethod 
		(this, sel);
	worldRaster.setColormap (colormap);
	worldRaster.setZoomFactor (zoomFactor);
	worldRaster.setWidth$Height((modelSwarm.getWorld()).getSizeX(),
				    (modelSwarm.getWorld()).getSizeY());
	worldRaster.setWindowTitle ("Food World");

	// This instructs the raster to digest all the information we
	// have just given it and to initialize itself for display.
	worldRaster.pack();


	// Now create a Value2dDisplay, an object that will display an
	// arbitrary 2-dimensional value array, in this case our
	// foodspace, on the raster widget.  Think of the foodspace
	// lattice overlaying the raster.  Remember that we have set
	// the colormap such that cells with no food (0's) will be
	// displayed in black and cells with food (1's) will be
	// displayed in red.  We use the Value2dDisplay implementation
	// class and give it a zone, the raster, the colormap and the
	// foodspace.
	foodDisplay = new Value2dDisplayImpl(getZone(), worldRaster, 
					     colormap,
					     modelSwarm.getFood());

	// Also create an Object2dDisplay that will display the bugs
	// on the raster, giving it the raster, the grid on which the
	// objects (bugs) are located, and the draw message to the
	// bugs.  (The Object2dDisplay relies on the objects to send
	// their own draw messages to the raster.)  Once the
	// Object2dDisplay is created, we give it the list of bugs to
	// which the draw message needs to be sent.  Again, think of
	// the bugspace as overlaying the raster.)
	sel = SwarmUtils.getSelector("SimpleBug", "drawSelfOn");
	bugDisplay = new Object2dDisplayImpl(getZone(), worldRaster, 
					     modelSwarm.getWorld(), sel);
	bugDisplay.setObjectCollection(modelSwarm.getBugList());

	// Finally, tell the Raster to send mouse clicks to the
	// bugDisplay, allowing the user to right-click on the display
	// to probe the individual bugs. (The right button is button
	// number 3.)
	sel = SwarmUtils.getSelector(bugDisplay, "makeProbeAtX$Y");
	worldRaster.setButton$Client$Message(3, bugDisplay, sel);

       	return this;
    }

    public Object buildActions()
    {
	Selector sel;
	ActionGroupImpl displayActions;

	// Use the parent class to begin the process.
	super.buildActions();

	// Call on the model swarm to build and schedule its actions.
	modelSwarm.buildActions();

	// Create an ActionGroup for display.  This is a list of
	// display actions that we want to occur at each step in
	// simulation time. First we tell the foodDisplay and the
	// bugDisplay to display themselves on the raster widget, and
	// then tell the raster widget to display itself on the
	// screen.  "doTkEvents", is required at the end to make
	// everything happen.  We then check to see if modelSwarm has
	// told us to stop the simulation.
	displayActions = new ActionGroupImpl(getZone());

	sel = SwarmUtils.getSelector(foodDisplay, "display");
	displayActions.createActionTo$message(foodDisplay, sel);

	sel = SwarmUtils.getSelector(bugDisplay, "display");
	displayActions.createActionTo$message(bugDisplay, sel);

	sel = SwarmUtils.getSelector(worldRaster, "drawSelf");
	displayActions.createActionTo$message(worldRaster, sel);

	sel = SwarmUtils.getSelector(this, "updateSimulatedTime");
	displayActions.createActionTo$message(this, sel);

	sel = SwarmUtils.getSelector(Globals.env.probeDisplayManager,
				     "update");
	displayActions.createActionTo$message(Globals.env.probeDisplayManager,
					      sel);

	sel = SwarmUtils.getSelector(getActionCache(), "doTkEvents");
	displayActions.createActionTo$message(getActionCache(), sel);

	sel = SwarmUtils.getSelector(this, "checkForDone");
	displayActions.createActionTo$message(this, sel);

	// Finally, put the ActionGroup into a display schedule.
	displaySchedule = new ScheduleImpl(getZone(), displayFrequency);
	displaySchedule.at$createAction(0, displayActions);

	return this;
    }

    // Activate the schedules so that they are ready to run.  The
    // swarmContext argument is the zone in which the ObserverSwarm is
    // activated.  Typically the ObserverSwarm is the top-level swarm,
    // so it is activated in "null".  The other (sub)swarms and
    // schedules will be activated inside of the ObserverSwarm
    // context.
    public Activity activateIn(Swarm swarmContext)
    {
	// Use the parent class to activate ourselves in the context
	// passed to us.
	super.activateIn(swarmContext);

	// Now activate the model swarm in the ObserverSwarm context.
	modelSwarm.activateIn(this);

	// Then activate the ObserverSwarm schedule in the
	// ObserverSwarm context.
	displaySchedule.activateIn(this);

	// Finally, return the activity we have built - the thing that
	// is ready to run.
	return getActivity();
    }

    // This method checks each period to see if the simulation is
    // done.  It is called at the end of each period.
    public void checkForDone()
    {
	if (simulationFinished)
	    {
	    // The simulation is over. Presumably we got here because
	    // the user did not QUIT when told to do so after the
	    // modelSwarm activity was terminated.  We therefore chide
	    // her and press QUIT ourselves.
	    System.out.println("I said to QUIT!");
	    modelSwarm.getActivity().terminate();
	    modelSwarm.drop();
	    getControlPanel().setStateQuit();
	    }

	else if (modelSwarm.getActivity().getStatus() ==
		 Globals.env.Completed)
	    {
	    // modelSwarm has signaled us that the simulation is
	    // finished by terminating itself.  (ObserverSwarm sees
	    // this as Completed".)  Press the STOP button on the
	    // control panel.  Pressing STOP rather than QUIT leaves
	    // the raster window and the control panel on the display
	    // so that the user can look at the results of the
	    // simulation.  (Those windows will disappear when the
	    // user presses QUIT.)  We also set a flag to indicate
	    // that the simulation is over, in case the user presses
	    // START or NEXT instead of QUIT.
	    simulationFinished = true;
	    System.out.println("The simulation ended after "
			       + Globals.env.getCurrentTime()
			       + " periods.");
	    System.out.println("Press QUIT when ready.");
	    getControlPanel().setStateStopped();
	    }
    }

    // We want simulatedTime to keep track of the current time so that
    // we can display it in our custom probe map.
    public void updateSimulatedTime()
    {
	simulatedTime = Globals.env.getCurrentTime();
	numberOfBugs = modelSwarm.numberOfBugs;
    }

    // This is a method given to the raster object to tell it what to
    // do in the event of an untimely death.
    public Object _worldRasterDeath_ (Object caller)
    {
	worldRaster.drop ();
	worldRaster = null;
	return this;
    }
}
