#import <objectbase.h>
#import <objectbase/Swarm.h>
#import <space.h>
#import <activity.h>
#import <collections.h>

@interface ForestModelSwarm : Swarm {
  int speciesNumber ;
  int worldSize ;
  int freqLStrikes;					  
  
  id theForest, fire, theFireGrid ;
  id speciesList ;

  id modelActions;				  
  id modelSchedule;
}

- populateWithSpecies: aSpecies ;
- (int)getSpeciesNumber ;
- getSpeciesList ;
- (int)getWorldSize ;

- printSpeciesPopulations;

- getTheForest ;
- getTheFireGrid ;

+ createBegin: aZone;
- createEnd;			
- buildObjects;
- buildActions;
- activateIn: swarmContext;

@end



















