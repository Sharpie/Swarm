// ExperSwarm.java
// The top-level ExperSwarm

import swarm.Globals;
import swarm.Selector;

import swarm.defobj.Zone;
import swarm.defobj.ZoneImpl;

import swarm.objectbase.Swarm;
import swarm.objectbase.EmptyProbeMap;
import swarm.objectbase.EmptyProbeMapImpl;

import swarm.simtoolsgui.GUISwarm;
import swarm.simtoolsgui.GUISwarmImpl;

import swarm.analysis.EZGraphImpl;

import swarm.activity.ActionGroupImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;

public class ExperSwarm extends GUISwarmImpl
{
    public int runningModel = 0;

    ParameterManager parameterManager;
    ModelSwarm modelSwarm;
    Zone modelZone;
    EZGraphImpl resultGraph;
    ScheduleImpl experSchedule;
    boolean weAreDone = false;

    // The contructor for an ExperSwarm.
    public ExperSwarm(Zone azone)
    {
	// Use the parent class constructor.
	super(azone);

	// Build a custon probe map.
	EmptyProbeMapImpl experProbeMap = 
	    new EmptyProbeMapImpl(azone, getClass());

	// Now add probes for the variables we wish to probe.
	experProbeMap.addProbe(getProbeForVariable("runningModel"));

	// Install our probe map into the probeLibrary.
	Globals.env.probeLibrary.setProbeMap$For(experProbeMap, 
						 getClass());
    }

    // Build the objects for an ExperSwarm
    public Object buildObjects()
    {
	Zone parameterZone;

	// Call the constructor for the parent class.
	super.buildObjects();

	// Build the Parameter Manager.
	parameterManager = 
	    (ParameterManager)Globals.env.lispAppArchiver.getObject(
					         "parameterManager");

	// Create an EZGraph object to display the results of the
	// runs.
	resultGraph = new EZGraphImpl(getZone(), "Model Run Times",
				      "Model Number", "Run Time",
				      "resultGraph");

	// Create a sequence to track model run times for the
	// EZGraph. Since we keep changing models, we feed EZGraph
	// with our own method, "getModelTime", which will probe the
	// correct instance of ModelSwarm.
	Selector sel = SwarmUtils.getSelector(this, "getModelTime");
	resultGraph.createSequence$withFeedFrom$andSelector("runTime", 
							    this, sel);

	// Dispay the probe map for this ExperSwarm.
	Globals.env.createArchivedProbeDisplay(this, "experSwarm");

	// Wait for the user to press START.
	getControlPanel().setStateStopped();

	// Have parameterManager open the output
	// archiver. parameterManager needs a valid Zone in which to
	// do this.
	parameterManager.openArchiver(getZone());

	return this;
    }

    // Build the actions for ExperSwarm.
    public Object buildActions()
    {
	Selector sel;
	ActionGroupImpl experActions;

	// Call upon the parent class.
	super.buildActions();

	// Create and fill the ActionGroup that specifies what actions
	// our ExperSwarm performs.
	experActions = new ActionGroupImpl(getZone());

	// Build, run, analyze and drop the model.
	sel = SwarmUtils.getSelector(this, "buildModel");
	experActions.createActionTo$message(this, sel);
	sel = SwarmUtils.getSelector(this, "runModel");
	experActions.createActionTo$message(this, sel);
	sel = SwarmUtils.getSelector(this, "doStats");
	experActions.createActionTo$message(this, sel);
	sel = SwarmUtils.getSelector(this, "showStats");
	experActions.createActionTo$message(this, sel);
	sel = SwarmUtils.getSelector(this, "logResults");
	experActions.createActionTo$message(this, sel);
	sel = SwarmUtils.getSelector(this, "dropModel");
	experActions.createActionTo$message(this, sel);

	// Check to see if the experiment is over.
	sel = SwarmUtils.getSelector(this, "checkToStop");
	experActions.createActionTo$message(this, sel);

	// Update the probe display and flush the screen buffer.
	sel = SwarmUtils.getSelector(Globals.env.probeDisplayManager,
				     "update");
	experActions.createActionTo$message(Globals.env.probeDisplayManager,
					    sel);
	sel = SwarmUtils.getSelector(getActionCache(), "doTkEvents");
	experActions.createActionTo$message(getActionCache(), sel);

	// Finally, put the ActionGroup into a schedule.
	experSchedule = new ScheduleImpl(getZone(), 1);
	experSchedule.at$createAction(0, experActions);

	return this;
    }

    // Activate everything to get an Activity which is ready to run.
    public Activity activateIn(Swarm swarmContext)
    {
	// Call on the parent class.
	super.activateIn(swarmContext);

	// Now activate the schedule in ourselves.
	experSchedule.activateIn(this);

	// Return the activity we have built.
	return getActivity();
    }

    // Here are the methods for building, running, examining and
    // dropping the ModelSwarms.

    // Build a ModelSwarm.
    public Object buildModel()
    {
	// Before beginning a new model, we check to see if the
	// experiment is over. The user (by mistake) may have pressed
	// START or NEXT rather than QUIT.
	if (weAreDone)
	    {
	    getActivity().terminate();
	    System.out.println("The experiment is over.");
	    getControlPanel().setStateQuit();
	    return null;
	    }

	// Create a new ModelSwarm and increment the model number.
	modelZone = new ZoneImpl(getZone());
	modelSwarm = new ModelSwarm(modelZone);
	++runningModel;

	// If this is the first model run, build a custom probe map.
	if (runningModel == 1)
	    {
	    EmptyProbeMapImpl modelProbeMap = 
		new EmptyProbeMapImpl(getZone(), modelSwarm.getClass());

	    // Here we "check out" from the Probe Library, probes on
	    // the modelSwarm's model parameters.
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"worldXSize", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"worldYSize", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"seedProb", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"bugDensity", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"bugHardiness", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"numberOfBugs", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
			 "currentNumberOfBugs", modelSwarm.getClass()));
	    modelProbeMap.addProbe(
	        Globals.env.probeLibrary.getProbeForVariable$inClass(
				"simulatedTime", modelSwarm.getClass()));

	    // Install our probe map into the probeLibrary.
	    Globals.env.probeLibrary.setProbeMap$For(modelProbeMap, 
						     modelSwarm.getClass());
	    }

	// Ask the Parameter Manager to initialize this new instance
	// of the model.
	parameterManager.initializeModel(modelSwarm);

	// Create a probe display for the new modelSwarm and force the
	// probeDisplayManager to flush it to the screen.
	Globals.env.createArchivedProbeDisplay(modelSwarm,
					       "modelSwarm");
	Globals.env.probeDisplayManager.update();

	// Then build and activate the new modelSwarm.
	modelSwarm.buildObjects();
	modelSwarm.buildActions();
	modelSwarm.activateIn(null);

 	return this;
    }

    // We have built the model and activated it. Now run it. When it
    // has terminated, control will return here.
    public Object runModel()
    {
	System.out.println("Starting model number " + runningModel);
	modelSwarm.getActivity().run();
	System.out.println("Model " + runningModel + " is done.");

	return this;
    }

    // Collect statistics on the model. In this case, we just report
    // the run time to the console.
    public Object doStats()
    {
	System.out.println("This run went for " + modelSwarm.getTime()
			   + " periods with " + modelSwarm.getBugs()
			   + " bugs.");

	return this;
    }

    // This method feeds the EZGraph "runTime" sequence. This always
    // points to the current ModelSwarm.
    public int getModelTime()
    {
	return modelSwarm.getTime();
    }

    // This method tells the parameterManager to log the results of
    // this run, passing it the model number and the run time.
    public Object logResults()
    {
	parameterManager.printParameters(runningModel, 
					 modelSwarm.getTime());

	return this;
    }

    // This method forces the EZGraph to update itself on the display.
    public Object showStats()
    {
	resultGraph.step();

	return this;
    }

    // We've finished with this model.  We drop its probe display,
    // then its Activity, and finally its Zone.  All traces should
    // have disappeared.
    public Object dropModel()
    {
	Globals.env.probeDisplayManager.dropProbeDisplaysFor(modelSwarm);
	modelSwarm.getActivity().drop();
	modelZone.drop();

	return this;
    }

    // Here we ask parameterManager to generate the next set of
    // parameters. If parameterManager returns a null, it means that
    // we have completely traversed the parameter space and the
    // experiment is over.  We update the display one last time,
    // inform the user through the console, set a flag, and press the
    // STOP button on the control panel.
    public Object checkToStop()
    {
	if (parameterManager.stepParameters() == null)
	    {
	    Globals.env.probeDisplayManager.update();
	    getActionCache().doTkEvents();

	    System.out.println("We've traversed the parameter space.");
	    System.out.println("Press QUIT when ready.");

	    weAreDone = true;
	    getControlPanel().setStateStopped();
	    }

	return this;
    }
}





