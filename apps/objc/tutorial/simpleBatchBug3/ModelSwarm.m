// ModelSwarm.m					SimpleBug app

#import "ModelSwarm.h"
#import <random.h>
#import <activity.h>
#import <collections.h>
#import "Parameters.h"   //needed to get info from arguments
#import "Output.h"

@implementation ModelSwarm  

// These methods provide access to the objects inside the ModelSwarm.
// These objects are the ones visible to other classes via message call.
// In theory we could just let other objects use Probes to read our state,
// but message access is frequently more convenient.

- getBugList
{
  return bugList;
}

- getWorld
{
  return world;
}

- getFood
{
  return food;
}

//  We used to have +createBegin: here, but no longer!


- createEnd
{
  return [super createEnd];
}

- resetParameters
{
  // These retrieve values from the Parameters object
  worldXSize = getInt ((Parameters *)arguments, "worldXSize");
  worldYSize = getInt ((Parameters *)arguments,"worldYSize");
  seedProb   = [(Parameters *)arguments getSeedProb];
  bugDensity = [(Parameters *)arguments getBugDensity];
  return self;
}

// Check if there is an input file, if so use it to create the
// food object. Otherwise, create a new food space as usual.
- createFoodSpace
{
  char * infile =  [(Parameters*)arguments getFilename];
  if ( infile != NULL)
    {
      id archiver = [LispArchiver create: [self getZone] setPath: infile];
      food = [archiver getObject: "food"];
      [archiver drop];
    }
  else
    {
      food = [FoodSpace createBegin: self];
      [food setSizeX: worldXSize Y: worldYSize];
      food = [food createEnd];
      
      [food seedFoodWithProb: seedProb];
    }
  return self;
}

// Check if there is an input file.  If there is one,
// it is necessary to read in the list of bugs, and then
// "re-initialize" the bugs by setting world and food,
// and then it is necessary to have the world put the
// bugs at their locations.

- createBugList
{
  char * infile =  [(Parameters*)arguments getFilename];
  if ( infile != NULL)
    {
      id <Index> index;
      Bug * aBug;
      id archiver = [LispArchiver create: [self getZone] setPath: infile];
      bugList = [archiver getObject: "bugList"];
      [archiver drop];

      index = [bugList begin: self];
      for ( aBug = [index next]; [index getLoc]==Member; aBug = [index next])
	{
	  [aBug setWorld: world Food: food];

	  [world putObject: aBug atX: [aBug getX] Y: [aBug getY] ];
	}
    }
  else
    {
      int x,y;
      Bug * aBug;
      bugList = [List create: self];
      
      for (y = 0; y < worldYSize; y++)
	for (x = 0; x < worldXSize; x++) 
	  if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < bugDensity)
	    {
	      aBug = [Bug createBegin: self];
	      [aBug setWorld: world Food: food];
	      aBug = [aBug createEnd];
	      [aBug setX: x Y: y];
	      
	      [bugList addLast: aBug];
	    }
    }
  printf ("Random seed is %u\n",[randomGenerator getInitialSeed]);
  return self;
}


- buildObjects
{

  // Set up the grid used to represent agent position
  // Grid2d enforces only 1 bug per site
  // This has to be done first, before bugs are created.
  world = [Grid2d createBegin: self];
  [world setSizeX: worldXSize Y: worldYSize];
  world = [world createEnd];
  [world fillWithObject: nil]; 

  // Then, create the food space and initialize it.
  
  [self createFoodSpace];

  // Now, create a bunch of bugs to live in the world

  [self createBugList];
  
  output = [Output create: self];
  [output setBugList: bugList];
  [output buildObjects];

  
  return self;
}




- buildActions
{
  // Create the list of simulation actions. We put these in an action
  // group, because we want these actions to be executed in a specific
  // order, but these steps should take no (simulated) time. The
  // M(foo) means "The message called <foo>". You can send a message
  // To a particular object, or ForEach object in a collection.
  
  modelActions = [ActionGroup create: self];
  [modelActions createActionForEach: bugList    message: M(step)];
  [modelActions createActionTo: output message: M(step)];

  // Then we create a schedule that executes the modelActions. modelActions
  // is an ActionGroup, by itself it has no notion of time. In order to
  // have it executed in time, we create a Schedule that says to use
  // the modelActions ActionGroup at particular times.
  // This schedule has a repeat interval of 1, it will loop every time step.
  // The action is executed at time 0 relative to the beginning of the loop.

  // This is a simple schedule, with only one action that is just
  // repeated every time. See mousetraps for more complicated schedules.

  modelSchedule = [Schedule createBegin: self];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createAction: modelActions]; 

  return self;
}

- activateIn: swarmContext
{
  // Here, we activate the swarm in the context passed in
  // Then we activate our schedule in ourselves

  [super activateIn: swarmContext];

  [modelSchedule activateIn: self];

  return [self getSwarmActivity];
}

- (void)drop
{
  [output drop];
}

- (BOOL)checkToStop
{
  if ([output checkToStop] == YES) return YES;
  return NO;
}


- lispArchive: (char *)inputName
{
  char dataArchiveName[100];
  if (!inputName)
    snprintf(dataArchiveName,100,"%s%d-%lu.scm","run",getInt((Parameters*)arguments,"run"),[(Parameters*)arguments getCurrentTime]);
  else
    snprintf(dataArchiveName,100,"%s-%d-%lu-%s.scm","run",getInt((Parameters*)arguments,"run"), [(Parameters*)arguments getCurrentTime]  ,inputName);
  {
    id dataArchiver = [LispArchiver create: [self getZone] setPath: dataArchiveName];
    
    [dataArchiver putShallow: "modelSwarm" object: self];
    [dataArchiver putShallow: "food" object: food];
    [dataArchiver putDeep: "bugList" object: bugList];

    [dataArchiver sync];
    [dataArchiver drop];
  }
  return self;
}

- (void)lispOutShallow: stream
{

  [stream catStartMakeInstance: "ModelSwarm"];
   
  [self lispSaveStream: stream Integer: "worldXSize" Value: worldXSize];
  [self lispSaveStream: stream Integer: "worldYSize" Value: worldYSize];

  [self lispSaveStream: stream Double: "seedProb" Value: seedProb];
  [self lispSaveStream: stream Double: "bugDensity" Value: bugDensity];

  [stream catEndMakeInstance];
}

@end



  


