// ParameterManager.java

import swarm.Globals;

import swarm.defobj.Zone;
import swarm.defobj.Archiver;
import swarm.defobj.LispArchiverImpl;
import swarm.defobj.HDF5ArchiverImpl;

import java.text.DecimalFormat;

public class ParameterManager
{
    // Declare the parameters for the experiment and their default
    // values. Note that only variables declared as public can be
    // serialized by the archivers.
    public int worldXSize = 80, worldYSize = 80;
    public double seedProb = 0.80;
    public double seedProbInc = 0.00;
    public double seedProbMax = seedProb;
    public double bugDensity = 0.01;
    public double bugDensityInc = 0.00;
    public double bugDensityMax = bugDensity;
    public int bugHardiness = 10;
    public int bugHardinessInc = 0;
    public int bugHardinessMax = bugHardiness;
    public boolean useLisp = true;

    // runTime is used to receive the runtime of the model most
    // recently run.
    public int runTime;

    double seedProbMin = seedProb;
    double bugDensityMin = bugDensity;
    int bugHardinessMin = bugHardiness;
    Archiver outputArchiver;

    // This is the constructor for a new ParameterManager.
    public ParameterManager()
    {
    }

    // Open the appropriate archiver for output in the Zone passed to
    // us.
    public Object openArchiver(Zone azone)
    {
	if (useLisp)
	    outputArchiver = new LispArchiverImpl(azone, 
						  "output.scm");
    	else
	    outputArchiver = new HDF5ArchiverImpl(azone, 
						  "output.hdf");

	return this;
    }

    // Give a new ModelSwarm its model parameters.
    public Object initializeModel(ModelSwarm theModel)
    {
	theModel.setParameters(worldXSize, worldYSize, seedProb, 
			       bugDensity, bugHardiness);

	return this;
    }

    // Step through the ranges of parameter values. When the entire
    // parameter space has been traversed, the experiment is over.
    // Close (drop) the archiver and return a null.
    public Object stepParameters()
    {
	double x;
	int i;

	if ((bugHardinessInc != 0) && 
	    ((i = bugHardiness + bugHardinessInc) <= bugHardinessMax))
	    {
	    bugHardiness = i;
	    return this;
	    }
	else if ((bugDensityInc != 0.0) && 
	    ((x = bugDensity + bugDensityInc) <= bugDensityMax))
	    {
	    bugDensity = x;
	    bugHardiness = bugHardinessMin;
	    return this;
	    }
	else if ((seedProbInc != 0.0) && 
	    ((x = seedProb + seedProbInc) <= seedProbMax))
	    {
	    seedProb = x;
	    bugDensity = bugDensityMin;
	    bugHardiness = bugHardinessMin;
	    return this;
	    }

	outputArchiver.drop();
	return null;
    }

    // Write the parameters and runTime for the current run to a log
    // file using the archiver passed to us. (Note that we format the
    // key to depict the model number as a three digit integer with
    // leading zeros.)
    public Object printParameters(int modelNumber, int time)
    {
	String key;

	runTime = time;

	DecimalFormat number = new DecimalFormat("model000");
	key = number.format(modelNumber);
	outputArchiver.putShallow$object(key, this);
	outputArchiver.sync();

	return this;
    }

}
