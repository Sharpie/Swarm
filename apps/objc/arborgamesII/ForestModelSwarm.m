#import "ForestModelSwarm.h"
#import "Forest.h"
#import "Fire.h"
#import "Tree.h"
#import "Species.h"
#import <random.h>
#import <simtools.h> // UName, ObjectLoader

@implementation ForestModelSwarm

- (int)getSpeciesNumber
{
  return speciesNumber ;
}

- getSpeciesList
{
  return speciesList ;
}

- (int) getWorldSize
{
  return worldSize ;
}

- getTheForest
{
  return theForest ;
}

- getTheFireGrid
{
  return theFireGrid ;
}

+ createBegin: aZone
{
  ForestModelSwarm * obj;
  id <ProbeMap> probeMap;

  obj = [super createBegin: aZone];

  obj->speciesNumber = 8 ;
  obj->worldSize = 100;
  obj->freqLStrikes = 3;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldSize"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "freqLStrikes"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "speciesNumber"
				    inClass: [self class]]];
  [probeLibrary setProbeMap: probeMap For: [self class]];
  
  return obj;
}

- createEnd
{
  return [super createEnd];
}

- populateWithSpecies: aSpecies
{
  int i, aNumber ;

  aNumber = [aSpecies getInitialPopulation] ;

  for (i = 0; i < aNumber; i++)
    {
      Tree *aTree;
      int age ;
      
      aTree = [aSpecies createTree: self];

      age = [uniformIntRand getIntegerWithMin: 0 withMax: [aSpecies getAgeLevel: 4]] ;
      
      [aTree setAge: age] ;
      
      if(age > [aSpecies getAgeLevel: 2])
        [aTree setSeedingCounter: 
                 (age - ([aSpecies getAgeLevel: 2] + 1)) 
               % [aSpecies getSeedPeriodicity]];
      
      [theForest addTree: aTree]; //let forest handle details of placement
     
    }
  
  return self ;
}

- buildObjects
{
  int i ;
  id theNamer, archiver ;
  
  [super buildObjects];

  theForest = [Forest createBegin: [self getZone]];
  [theForest setWorldSize: worldSize] ;
  theForest = [theForest createEnd] ;
  
  theFireGrid = [Discrete2d createBegin: [self getZone]] ;
  [theFireGrid setSizeX: worldSize Y: worldSize] ;
  theFireGrid = [theFireGrid createEnd] ;

  fire = [Fire create: [self getZone]] ;
  [fire setWorldSize: worldSize] ;
  [fire setForest: theForest] ; 
  [fire setFreqLStrikes: freqLStrikes] ;

  [fire setFireGrid: theFireGrid];
  [theForest setFireGrid: theFireGrid];

  theNamer = [UName create: [self getZone] setBaseName: "species"];
  speciesList = [Array create: [self getZone] setCount: speciesNumber];


  //Load the Species objects from the archive file
  //Note, this is the same as "createBegin" and "createEnd"
  archiver = [LispArchiver create: self setPath: "species.scm"];
  for(i = 0 ; i < speciesNumber ; i++)
    {
      Species *aSpecies = [archiver getWithZone: self key: [theNamer getNewName]];
      [aSpecies setModelSwarm: self] ;
      [aSpecies setForest: theForest] ;
      [aSpecies initWorldSize: worldSize];
      [speciesList atOffset: i put: aSpecies] ;  
    }
  
  for (i=0; i < speciesNumber; i++)
    [self populateWithSpecies: [speciesList atOffset: i]] ;



  [theForest setSpeciesList: speciesList];

  [archiver drop];

  [theNamer drop] ;

  return self;
}



- printSpeciesPopulations
{
  int k;
  fprintf(stderr,"Time = %lu Species Levels\n",getCurrentTime());
  
  for (k=0; k< speciesNumber; k++)
    {
      fprintf (stderr,"%d ",[[speciesList atOffset: k ] getCount ]);
    }
  fprintf(stderr,"\n \n");
  return self;
}


- buildActions 
{
  [super buildActions];
  
  modelActions = [ActionGroup create: [self getZone]];

  [modelActions createActionTo:      fire             message: M(step)];

  [modelActions createActionTo:      theForest        message: M(step)];
 
  [modelActions createActionTo: self message: M(printSpeciesPopulations)];
 
  modelSchedule = [Schedule createBegin: [self getZone]];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createAction: modelActions];

  return self;
}

- activateIn: swarmContext 
{
  [super activateIn: swarmContext];

  [modelSchedule activateIn: self];

  return [self getSwarmActivity];
}


@end



