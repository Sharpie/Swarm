// SimpleCbug

#import <simtools.h>
#import "Bug.h"

int
main (int argc, const char **argv)
{
  int worldXSize = 80;
  int worldYSize = 80;

  int xPos = 40;
  int yPos = 40;

  int i;

  id aBug;			// "id" is the default type for objects

  initSwarm(argc, argv);

  aBug = [Bug create: globalZone]; 	// Make us aBug please!
					// and initialize it
  [aBug setX: xPos Y: yPos];	
  [aBug setWorldSizeX: worldXSize Y: worldYSize];

  for(i = 0; i < 100; i++) {

    [aBug step]; 			// Tell it to act

  }

  return 0;

}
 
 




