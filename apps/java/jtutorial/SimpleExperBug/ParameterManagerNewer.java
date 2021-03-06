// ParameterManager.java

import swarm.Globals;
import swarm.Selector;

import swarm.defobj.Zone;
import swarm.defobj.ZoneImpl;
import swarm.defobj.Archiver;
import swarm.defobj.LispArchiverImpl;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.objectbase.SwarmObjectImpl;
import swarm.objectbase.EmptyProbeMap;
import swarm.objectbase.EmptyProbeMapImpl;

import swarm.simtoolsgui.GUISwarm;
import swarm.simtoolsgui.GUISwarmImpl;

import swarm.analysis.EZGraphImpl;

import swarm.activity.ActionGroupImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.Activity;

import swarm.space.Grid2dImpl;
import swarm.collections.ListImpl;

public class ParameterManager extends SwarmObjectImpl
{
    // Declare the parameters for the experiment and their default
    // values.
    public int worldXSize = 80, worldYSize = 80;
    public double seedProb = 0.80;
    public double seedProbInc = 0.00;
    public double seedProbMax = 1.00;
    public double bugDensity = 0.01;
    public double bugDensityInc = 0.03;
    public double bugDensityMax = 0.03;
    public int bugHardiness = 10;
    public int bugHardinessInc = 0;
    public int bugHardinessMax = 50;
    public boolean useLisp = true;

    // runTime is used to receive the runtime of the model most
    // recently run.
    int runTime;

    // This is the constructor for a new ParameterManager.
    public ParameterManager()
    {
	// Call on the constructor for the parent class.
	super();
    }


    // Give a new ModelSwarm its model parameters.
    public Object initializeModel(ModelSwarm theModel)
    {
	theModel.setParameters(worldXSize, worldYSize, seedProb, 
			       bugDensity, bugHardiness);

	return(this);
    }

    // Step through the ranges of parameter values in a very crude
    // way. If any parameter has reached its maximum value, the
    // experiment is over.  Return a null.
    public Object stepParameters()
    {
	seedProb += seedProbInc;
	bugDensity += bugDensityInc;
	bugHardiness += bugHardinessInc;

	if ((seedProb > seedProbMax) || (bugDensity > bugDensityMax)
	    || (bugHardiness > bugHardinessMax))
	    return null;

	return this;
    }

    // Write the parameters for the current run to a log file using
    // the LispArchiver.
    public Object printParameters(Archiver archive, 
				  int modelNumber, int time)
    {
	String key;

	runTime = time;
	key = "model" + modelNumber;
	archive.putShallow$object(key, this);
	archive.sync();

	return this;
    }
}
