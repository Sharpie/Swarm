// SimpleObjCbug2

#import <simtools.h>
#import "Bug.h"
#import "FoodSpace.h"

int
main (int argc, const char **argv)
{
  int worldXSize = 80;
  int worldYSize = 80;

  int xPos = 40;
  int yPos = 40;
  
  float seedProb = 0.5;		// Density of distribution of "food"

  int i;

  id aBug;
  id foodSpace;

  initSwarm (argc, argv);

  // Create and initialize a "food" space

  foodSpace = [FoodSpace createBegin: globalZone];
  [foodSpace setSizeX: worldXSize Y: worldYSize];
  foodSpace = [foodSpace createEnd];

  [foodSpace seedFoodWithProb: seedProb];  

  aBug = [Bug createBegin: globalZone]; 
  [aBug setWorldSizeX: worldXSize Y: worldYSize];
  [aBug setFoodSpace: foodSpace];
  aBug = [aBug createEnd];
  
  [aBug setX: xPos Y: yPos];

  for(i = 0; i < 100; i++) {

    [aBug step];

  }

  return 0;

}
 
 




