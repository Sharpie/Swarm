#import <collections.h>
#import <analysis.h>
#import <objectbase.h>
#import "ForestBatchSwarm.h"
#import "Forest.h"

@implementation ForestBatchSwarm

+createBegin: (id) aZone {
  ForestBatchSwarm * obj;
  
  obj = [super createBegin: aZone];

  obj->metricFrequency = 1;
  obj->forestFrequency = 0;
  obj->experimentDuration = 100 ;

  return obj;
}

-createEnd {
  return [super createEnd];
}

- buildObjects 
{
  id modelZone;
  int i;

  [super buildObjects];
  
  modelZone = [Zone create: [self getZone]]; 
  
  // use the built-in object lispAppArchiver to load modelSwarm parameters
  if ((forestModelSwarm = [lispAppArchiver getWithZone: modelZone key: "modelSwarm"])== nil)
    raiseEvent(InvalidOperation, "forestModelSwarm parameters missing");
  
 
  // Up through arborgames-2.2, the old fashioned ObjectLoader was used.
  // The newer approach is a Lisp based storage.  
  // In case you wonder how I created the *.scm file, here's how.
  // I ran the old fashioned ObjectLoader and then dumped out batch.scm.
  // Then I manually edited batch.scm to delete objects that are "nil"

  // Here's the old way
  // [ObjectLoader load: self
  //              fromAppDataFileNamed: "batch.setup"];
  //   [ObjectLoader load: forestModelSwarm
  //                 fromAppDataFileNamed: "experiment.setup"];

  // after that save the lisp output and then edit it.
  //   {
  //     id archiver = [LispArchiver create: self setPath: "arborgames.scm"];
  //     [archiver putShallow: "modelSwarm" object: forestModelSwarm];
  //     [archiver putShallow: "batchSwarm" object: self]; 
  //     [archiver sync];
  //     [archiver drop];
  //   }



  [forestModelSwarm buildObjects];
  speciesNumber = [forestModelSwarm getSpeciesNumber] ;
  speciesList = [forestModelSwarm getSpeciesList] ;

  if(forestFrequency){

    //We use the species color as an identifier when filing snapshots
    //of the forest...
    for(i = 0 ; i < speciesNumber ; i++)
      [[speciesList atOffset: i] setColorMapEntry: i + 1] ;

    forestNamer = [UName create: [self getZone] setBaseName: "snapshot"] ;
    forestFiler = [Int2dFiler createBegin: [self getZone]];
    [forestFiler setDiscrete2dToFile: 
       [[forestModelSwarm getTheForest] getTreeGrid: 1]] ;
    [forestFiler setValueMessage: M(getSpeciesIdentifier)] ;
    forestFiler = [forestFiler createEnd] ;
  }

  if(metricFrequency){
    speciesGraph = [EZGraph createBegin: [self getZone]] ;
    [speciesGraph setGraphics: 0] ;
    [speciesGraph setFileOutput: 1] ;
    speciesGraph = [speciesGraph createEnd] ;

    [speciesGraph createTotalSequence: "count"
                         withFeedFrom: speciesList
                          andSelector: M(stillActive)] ;

    entropyGraph = [EZGraph createBegin: [self getZone]] ;
    [entropyGraph setGraphics: 0] ;
    [entropyGraph setFileOutput: 1] ;
    entropyGraph = [entropyGraph createEnd] ;

    speciesEntropy = [Entropy createBegin: [self getZone]];
    [speciesEntropy setCollection: speciesList];
    [speciesEntropy setProbedSelector: M(getRelativeProportion)];
    speciesEntropy = [speciesEntropy createEnd];

    [entropyGraph createSequence: "entropy"
                    withFeedFrom: speciesEntropy
                     andSelector: M(getEntropy)] ;
  }

  return self;
}  

-buildActions {

  [super buildActions];
  
  [forestModelSwarm buildActions];

  if(metricFrequency){
    id metricActions ;

    metricActions = [ActionGroup create: [self getZone]];
  
    [metricActions createActionTo: speciesGraph message: M(step)];

    [metricActions createActionTo: speciesEntropy message: M(update)];
    [metricActions createActionTo: entropyGraph message: M(step)];

    metricSchedule = [Schedule createBegin: [self getZone]];
    [metricSchedule setRepeatInterval: metricFrequency];
    metricSchedule = [metricSchedule createEnd];
    [metricSchedule at: 0 createAction: metricActions];
  }

  if(forestFrequency){
    forestSchedule = [Schedule createBegin: [self getZone]];
    [forestSchedule setRepeatInterval: forestFrequency];
    forestSchedule = [forestSchedule createEnd];
    [forestSchedule at: 0 createActionTo: self message: M(snapShot)] ;
  }

  durationSchedule = [Schedule create: [self getZone]];
  [durationSchedule at: experimentDuration 
        createActionTo: self 
               message: M(drop)] ;

  return self;
}  

-activateIn: (id) swarmContext {
  [super activateIn: swarmContext];

  [forestModelSwarm activateIn: self];

  [metricSchedule activateIn: self];
  [forestSchedule activateIn: self];
  [durationSchedule activateIn: self];
  
  return [self getSwarmActivity];
}

-snapShot{
  [forestFiler fileTo: [forestNamer getNewName]] ;
  return self ;
}

-(void) drop {
  [getTopLevelActivity() terminate];
  [speciesGraph drop] ; // Ensures that files are
  [entropyGraph drop] ; // closed properly...
  return ;
}

-go {
  [[self getActivity] run];
  return [[self getActivity] getStatus];
}

@end
