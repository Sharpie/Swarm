// ExperSwarm.m					simpleExperBug

#import "ExperSwarm.h"

@implementation ParameterManager

  // The ParameterManager handles the parameter manipulation for
  //     the series of models run during the experiment


- initializeParameters
{
  // Initialize the parameterManager from the "experiment.setup" file

  [ObjectLoader load: self fromFileNamed: "experiment.setup"];

  // Now, build a custom probemap to display parameterManager variables

  pmProbeMap = [EmptyProbeMap createBegin: [self getZone]];
  [pmProbeMap setProbedClass: [self class]];
  pmProbeMap = [pmProbeMap createEnd];

  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "worldXSize"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "worldYSize"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "seedProb"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "bugDensity"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "seedProbInc"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "bugDensityInc"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "seedProbMax"
                                  inClass: [self class]]];
  [pmProbeMap addProbe: [probeLibrary getProbeForVariable: "bugDensityMax"
                                  inClass: [self class]]];

  [probeLibrary setProbeMap: pmProbeMap For: [self class]];

  // Finally, create a probeDisplay to show the parameterManager

  [probeDisplayManager createProbeDisplayFor: self
                       setWindowGeometryRecordName: "parameterManager"];

  return self;
}

- initializeModel: (ModelSwarm *)theModel
{
  [theModel setWorldXSize: worldXSize YSize: worldYSize];
  [theModel setSeedProb: seedProb bugDensity: bugDensity];
  return self;
}


- stepParameters
{
  seedProb += seedProbInc;
  bugDensity+= bugDensityInc;
 
  if ((seedProb > seedProbMax) || (bugDensity > bugDensityMax))
    return nil;

  return self;
}

- printParameters: (id <OutFile>)anOutFile
{
  [anOutFile putNewLine];
  [ObjectSaver save: self to: anOutFile withTemplate: pmProbeMap];
  [anOutFile putNewLine];

  return self;
}

@end

@implementation ExperSwarm

  // ExperSwarm manages creating, running, analyzing, and dropping
  // a series of models throughout an experimental run.


+ createBegin: aZone
{
  ExperSwarm *obj;
  id <ProbeMap> theProbeMap;

  // invoke our superClass createBegin to allocate ourselves.
  // obj is the allocated ExperSwarm
  
  obj = [super createBegin: aZone];

  // Fill in the relevant parameters 

  obj->numModelsRun = 0;

  // Build a customized probe map for ExperSwarm

  theProbeMap = [EmptyProbeMap createBegin: aZone];
  [theProbeMap setProbedClass: [self class]];
  theProbeMap = [theProbeMap createEnd];

  [theProbeMap addProbe: [probeLibrary getProbeForVariable: "numModelsRun"
                                   inClass: [self class]]];

  [probeLibrary setProbeMap: theProbeMap For: [self class]];

  return obj;		// We return the newly created ExperSwarm
}


- createEnd
{
  return [super createEnd];
}



- buildObjects
{
  // Create the objects used by the experiment swarm itself

  // First, let our superClass build any objects it needs 
 
  [super buildObjects];

  // Build the parameter manager

  parameterManager = [ParameterManager create: [self getZone]];
  [parameterManager initializeParameters];

  // Build a probeDisplay on ourself

  [probeDisplayManager createProbeDisplayFor: self
                       setWindowGeometryRecordName: "experSwarm"];

  // build the EZGraph for model results

  resultGraph = [EZGraph createBegin: [self getZone]];
  [resultGraph setWindowGeometryRecordName: "resultGraph"];
  [resultGraph setTitle: "Model Run Times"];
  [resultGraph setAxisLabelsX: "Model #" Y: "Run Time"];
  resultGraph = [resultGraph createEnd] ;

  // Create a sequence to track model run times.
  // Since we keep changing models, we feed from our own method
  // "getModelTime" which will probe the correct instance of 
  // ModelSwarm

  [resultGraph createSequence: "runTime"
                 withFeedFrom: self 
                  andSelector: M(getModelTime)];

  // Allow the user to alter experiment parameters

  [controlPanel setStateStopped];

  // Create the OutFile object to log the runs

  logFile = [OutFile create: [self getZone] withName: "log.file"];

  return self;
}



- buildActions
{
// Create the actions necessary for the experiment. This is where
// the schedule is built (but not run!)

  // First, let our superclass build any actions

  [super buildActions];

  // Create an ActionGroup for experiment: a bunch of things that occur in
  // a specific order, but in the same step of simulation time. 

  experActions = [ActionGroup create: [self getZone]];

  // Schedule up the methods to  build the model, run it, collect data,
  // display the data, log the results, and drop the model.  

  [experActions createActionTo: self	message: M(buildModel)];
  [experActions createActionTo: self	message: M(runModel)];
  [experActions createActionTo: self	message: M(doStats)];
  [experActions createActionTo: self	message: M(showStats)];
  [experActions createActionTo: self	message: M(logResults)]; 
  [experActions createActionTo: self	message: M(dropModel)];

 // Check to see if the experiment has ended (all the models have been run).

  [experActions createActionTo: self     message: M(checkToStop)];

  // Schedule the update of the probe display

  [experActions  createActionTo: probeDisplayManager message: M(update)];

  // Finally, schedule an update for the whole user interface code.

  [experActions createActionTo: actionCache          message: M(doTkEvents)];

  // Now make the experiment schedule. Note the repeat interval is 1
  
  experSchedule = [Schedule createBegin: [self getZone]];
  [experSchedule setRepeatInterval: 1];
  experSchedule = [experSchedule createEnd];
  [experSchedule at: 0 createAction: experActions];

  return self;
}  



- activateIn: swarmContext
{
// activateIn: - activate the schedules so they're ready to run.
// The swarmContext argument has to do with what we were activated *in*.
// Typically an ExperimentSwarm is the top-level Swarm, so swarmContext
// is "nil". The model we run will be independent of our activity.
// We will activate it in "nil" when we build it later.

  // First, activate ourselves (just pass along the context).

  [super activateIn: swarmContext];

  // Now activate our schedule in ourselves. This arranges for the
  // execution of the schedule we built.

  [experSchedule activateIn: self];

  return [self getActivity];
}


// --- End of ExperSwarm buildObjects, buildActions, and activateIn ---


// Now that we've built the Experiment Swarm, here are the methods for
// building, running, examining, and dropping the modelSwarms


- buildModel
{
  // Here, we create an instance of the model that we're managing. 
  // The model is a subswarm of the experiment. We create the model in
  // its own zone, so storage is segregated. We can drop everything
  // we create in the modelSwarm later by just dropping the modelZone

  modelZone = [Zone create: [self getZone]];
  modelSwarm = [ModelSwarm create: modelZone];

  // If this is the first model, create a custom probeMap for modelSwarm 
  // and construct a graph displaying model results

  if (numModelsRun == 0)
    {
      // Build a customized probe map for the class ModelSwarm
      
      modelProbeMap = [EmptyProbeMap createBegin: [self getZone]];
      [modelProbeMap setProbedClass: [modelSwarm class]];
      modelProbeMap = [modelProbeMap createEnd];
      
      // Add in a bunch of variables, one per simulation parameter
      
      [modelProbeMap addProbe: [probeLibrary getProbeForVariable: "worldXSize"
                                             inClass: [modelSwarm class]]];
      [modelProbeMap addProbe: [probeLibrary getProbeForVariable: "worldYSize"
                                             inClass: [modelSwarm class]]];
      [modelProbeMap addProbe: [probeLibrary getProbeForVariable: "seedProb"
                                             inClass: [modelSwarm class]]];
      [modelProbeMap addProbe: [probeLibrary getProbeForVariable: "bugDensity"
                                             inClass: [modelSwarm class]]];
      // Now install our custom probeMap into the probeLibrary.
      
      [probeLibrary setProbeMap: modelProbeMap For: [modelSwarm class]];
      
    } 		// endif
  
  // Now, we invoke the parameterManager to initialize the model 
  
  [parameterManager initializeModel: modelSwarm];

  // Now we create a probeDisplay for this model instance

  [probeDisplayManager createProbeDisplayFor: modelSwarm];

  // Let the modelSwarm build its objects and actions and activate
  // it in "nil", giving us a new activity. We don't start it here...
  // we will start models from the ExperSwarm schedule.
  
  [modelSwarm buildObjects];
  [modelSwarm buildActions];
  [modelSwarm activateIn: nil];

  return self;
}  


- runModel
{
  // We have built the model and activated it - here is where we run it.
  // When it has terminated, control will return here.

  printf("\nStarting model %d \n", numModelsRun+1);

  [[modelSwarm getActivity] run];

  printf("Model %d is done \n", numModelsRun+1);

  numModelsRun++;               // increment count of models

  return self;
}


- doStats
{
  // Collect a datapoint on the model

  modelTime =  [modelSwarm getTime];
  printf("Length of this run = %d \n", modelTime);

  return self;
}


- (int)getModelTime
{
  // This method feeds the EZGraph "runTime" sequence  
  // this always points to the current modelSwarm 

   return [modelSwarm getTime];
}


- logResults
{
  // This uses the OutFile object to log the parameters 
  //   and results of a run to the file "log.file"

  [logFile putString: "--------------------------------\n\n"];

  [logFile putString: "Model # "]; 
  [logFile putInt: numModelsRun];

  [logFile putNewLine];

  // have the parameterManager log its state
  [parameterManager printParameters: logFile];

  [logFile putNewLine];

  [logFile putString: "Time for this run = "];
  [logFile putInt: modelTime];

  [logFile putNewLine];

  return self;
}


- showStats
{
  [resultGraph step];			// step the result Graph
  return self;
}


- dropModel
{
  // The model has finished and we've extracted the data we need
  // from it. We drop the probeDisplay for it, drop its activity,
  // and then drop the Zone we created it in, which drops all
  // of the objects built by modelSwarm

  [probeDisplayManager dropProbeDisplaysFor: modelSwarm];
  [[modelSwarm getActivity] drop];
  [modelZone drop];

  return self;
}


- checkToStop
{
  // If all the models have run, time to quit!

  // If we step the parameterManager and it exceeds the parameter bounds,
  // it returns nil, so we have stepped through all the models and
  // we can quit the experiment. We first make sure that the
  // displays are updated, then we drop the logFile to close it,
  // and set the controlPanel to "stop".

  if (!([parameterManager stepParameters]))
    {
      
      [probeDisplayManager update];
      [actionCache doTkEvents];
      
      printf("\n All the models have run!\n");
      
      [logFile drop];
      [controlPanel setStateStopped];
    }
  
  return self;
}


@end
