// ExperSwarm.h					simpleExperBug

// The ExperSwarm is a swarm that manages multiple invocations of a
// model. 

#import "ModelSwarm.h"
#import <simtoolsgui/GUISwarm.h>
#import <objectbase/SwarmObject.h>
#import <objectbase.h>
#import <analysis.h>
#import <defobj.h> // Archiver

// First, the interface for the ParameterManager

@interface ParameterManager: SwarmObject
{
  unsigned worldXSize;
  unsigned worldYSize;

  float seedProb;
  float bugDensity;

  float seedProbInc;
  float bugDensityInc;

  float seedProbMax;
  float bugDensityMax;

  unsigned time;
}

- (void)initializeParameters;
- (void)initializeModel: theModel;
- (BOOL)stepParameters;
- (void)printParameters: (id <Archiver>)archiver number: (unsigned)sn time: (unsigned)tv;

@end




// Now, the Experiment Swarm Interface

@interface ExperSwarm: GUISwarm
{
  unsigned modelTime;				// model run time
  unsigned numModelsRun;			// number of models run

  id experActions;				// schedule data structs
  id experSchedule;

  ModelSwarm *modelSwarm;			// the Swarm we're iterating 

  ParameterManager *parameterManager; 		// An object to manage model
						// parameters

  id <Archiver> archiver;                       // for storing output

 // Display objects, widgets, etc.

  id <EZGraph> resultGraph;			// graphing widget
  id <ProbeMap> modelProbeMap;			// the ProbeMap for the modelSwarm
}

// Methods overriden to make the the Experiment Swarm

+ createBegin: aZone;

- buildObjects;
- buildActions;
- activateIn: swarmContext;			// Context is self (ObserverSwarm).

@end
