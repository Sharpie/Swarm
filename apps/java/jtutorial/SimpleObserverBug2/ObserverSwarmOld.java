// ObserverSwarm.java The observer swarm is collection of objects that
// are used to run and observe the ModelSwarm that actually comprises
// the simulation.

import swarm.Globals;
import swarm.Selector;

import swarm.defobj.Zone;
import swarm.defobj.ZoneImpl;

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
import swarm.objectbase.VarProbe;
import swarm.objectbase.VarProbeImpl;
import swarm.objectbase.VarProbeC;
import swarm.objectbase.VarProbeCImpl;

import swarm.collections.ListImpl;


public class ObserverSwarm extends GUISwarmImpl
{
    // Declare the display parameters and their default values.
    public int displayFrequency = 1;
    public int simulatedTime = 0;

    // Declare other variables local to ObserverSwarm.
    ModelSwarm modelSwarm;
    ZoomRaster worldRaster;
    Value2dDisplay foodDisplay;
    Object2dDisplay bugDisplay;
    ScheduleImpl displaySchedule;
    EmptyProbeMapImpl probeMap;

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
	probeMap = new EmptyProbeMapImpl(azone, getClass());

	// Now add probes for the variables we wish to probe, using
	// the method in our SwarmUtils class.
	probeMap.addProbe(getProbeForVariable("displayFrequency"));
	probeMap.addProbe(getProbeForVariable("simulatedTime"));

	// And finaly install our probe map into the probeLibrary.
	// Note that this library object was automatically created by
	// initSwarm.
	Globals.env.probeLibrary.setProbeMap$For(probeMap, getClass());

    }

    // This is a method given to the raster object to tell it what to
    // do in the event of an untimely death.
    public Object _worldRasterDeath_ (Object caller)
    {
	worldRaster.drop ();
	worldRaster = null;
	return this;
    }
    
    // The buildObjects method.
    public Object buildObjects()
    {
	Zone modelZone;
	Colormap colormap;
	Selector sel;

	// Use the parent class to initialize the process.
	super.buildObjects();

	// First we create the model that we're actually observing.
	// The model will be a subswarm of the observer.  We create
	// the model in its own zone so that its storage is
	// segregated.  Note that we are reading the model parameters
	// from a file and so use the List Archiver.
        // "modelSwarm", the second argument to getWithZone$key, is
        // the key in the .scm file which contains the values of model
        // parameters in the ModelSwarm class: worldXSize, worldYSize,
        // stepSize, seedProb and bugDensity.

        modelZone = new ZoneImpl(getZone());
	modelSwarm = 
	    (ModelSwarm)Globals.env.lispAppArchiver.getWithZone$key(
				 modelZone, "modelSwarm");

	// Now create probe objects on the model and on ourselves.
	// This provides a simple interface to allow the user to
	// change the model and simulation parameters.  Note that the
	// createArchivedProbeDisplay() method is provided by the Swarm
	// environment, set up by the call to initSwarm().
	Globals.env.createArchivedProbeDisplay(modelSwarm,
						"modelSwarm");
	Globals.env.createArchivedProbeDisplay(this, "observerSwarm");


	// Instruct the control panel to wait for a button event: we
	// halt here until someone hits a control panel button.  Now
	// that we are using probes, the user can set the model and
	// simulation parameters in the ModelSwarm and ObserverSwarm
	// windows.
	getControlPanel().setStateStopped();
        
	// OK - the user has pressed a button.  Now we're ready to
	// start.

	// Allow the model swarm to build its objects.
 	modelSwarm.buildObjects();

	// Now build the display objects.

	// First, create a colormap.  This is a global resource which
	// is used by lots of different objects. Then set the three
	// colors we will be using.  Since the FoodSpace grid uses one
	// to indicate a cell with food and zero to indicate a cell
	// without food, FoodSpace cells will be displayed in red or
	// black depending on whether they contain food or not.  We'll
	// use green to indicate the location of our bugs.
	colormap = new ColormapImpl(getZone());
	colormap.setColor$ToName((byte)0, "black");
	colormap.setColor$ToName((byte)1, "red");
	colormap.setColor$ToName((byte)2, "green");


	// Now go in to the bugs in the model and set their colors to
	// green (2).  We do this by getting the list of bugs and
	// interating through it.
	ListImpl bugList = modelSwarm.getBugList();
	for (int i = 0; i < bugList.getCount(); i++)
	    {
	    Simplebug bug = (Simplebug)bugList.atOffset(i);
	    bug.setBugColor((byte)2);
	    } 

	// Next, create a 2d "Raster" window for display, setting its size,
	// zoom factor and title.
	worldRaster = new ZoomRasterImpl (getZone(), "worldRaster");
	sel = SwarmUtils.getSelector(this, "_worldRasterDeath_");
	worldRaster.enableDestroyNotification$notificationMethod 
		(this, sel);
	worldRaster.setColormap (colormap);
	worldRaster.setZoomFactor (8);
	worldRaster.setWidth$Height((modelSwarm.getWorld()).getSizeX(),
				    (modelSwarm.getWorld()).getSizeY());
	worldRaster.setWindowTitle ("Food World");
	// Draw the window.
	worldRaster.pack();


	// Now create a Value2dDisplay, a special object that will
	// display arbitrary 2d value arrays on a given Raster widget.
	// Note that we use a convenient form of the constructor that
	// takes all these objects as arguments.
	foodDisplay = new Value2dDisplayImpl(getZone(), worldRaster, 
					     colormap,
					     modelSwarm.getFood());

	// Also create an Object2dDisplay that will display the bugs
	// on the Raster, giving it the Raster, the grid on which the
	// objects (bugs) are located, and the draw message.  Once
	// created, give the Object2dDisplay the list of bugs to
	// draw.
	sel = SwarmUtils.getSelector("Simplebug", "drawSelfOn");
	bugDisplay = new Object2dDisplayImpl(getZone(), worldRaster, 
					     modelSwarm.getWorld(), sel);
	bugDisplay.setObjectCollection(modelSwarm.getBugList());

	// Finally, tell the Raster to send mouse clicks to the
	// bugDisplay, allowing the user to right-click on the display
	// to probe the individual bugs.
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

	// Create an ActionGroup for updating the various display
	// elements.  This is a bunch of things that occur in a
	// specific order, but at one step in simulation time.  Note
	// that the last action is crucial.  Without the "doTkEvents"
	// message to the ActionCache, no graphics will update and the
	// control panel will be dead.  It's best to put this at the
	// end of the list of display updates.
	displayActions = new ActionGroupImpl(getZone());

	sel = SwarmUtils.getSelector(foodDisplay, "display");
	displayActions.createActionTo$message(foodDisplay, sel);

	sel = SwarmUtils.getSelector(bugDisplay, "display");
	displayActions.createActionTo$message(bugDisplay, sel);

	sel = SwarmUtils.getSelector(worldRaster, "drawSelf");
	displayActions.createActionTo$message(worldRaster, sel);

	sel = SwarmUtils.getSelector(this, "updateSimulatedTime");
	displayActions.createActionTo$message(this, sel);

	sel = SwarmUtils.getSelector(Globals.env.probeDisplayManager, "update");
	displayActions.createActionTo$message(Globals.env.probeDisplayManager, sel);

	sel = SwarmUtils.getSelector(getActionCache(), "doTkEvents");
	displayActions.createActionTo$message(getActionCache(), sel);

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

    public void updateSimulatedTime()
    {
	simulatedTime = Globals.env.getCurrentTime();
    }

}
