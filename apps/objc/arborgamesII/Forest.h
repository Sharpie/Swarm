#import <objectbase/SwarmObject.h>

@interface Forest: SwarmObject
{
  id youngTreeGrid;
  id matureTreeGrid;
  id speciesList;
#ifdef USELISTS
  id youngTreeList;
  id matureTreeList;
#endif
  id fireGrid;
  int worldSize;
}

- setWorldSize: (int)aSize;
- (int)getWorldSize;
- createEnd;
- step;
- getTreeType: (int) i AtX: (int)xVal Y: (int)yVal;
- addTree: aTree atX: (int)xVal Y: (int)yVal;
- addTree: aTree Type: (int)i atX: (int)xVal Y: (int)yVal;
- addTree: aTree;
- getTreeGrid: (int)i;
#ifdef USELISTS
- getTreeList: (int)i;
#endif
- destroyTreesAtX: (int)theX Y: (int)theY;


- fireAtX: (int)theX Y: (int)theY Heat: (int)generatedHeat;

- setSpeciesList: aList;
- setFireGrid: aGrid;

- processSpeciesGermination;
- processSpeciesSeeds;

- processMatureTrees;
- processYoungTrees;
- updatePopulationRecords;

@end
