// SimpleCbug

#import <simtools.h>

int
main(int argc, char ** argv) {

  int worldXSize = 80;			// Maximum X value
  int worldYSize = 80;			// Maximum Y value

  int xPos = 40;			// Bug's starting position
  int yPos = 40;

  int i;

  initSwarm(argc, argv);		// Always first in Swarm main.m 

  printf("I started at X = %d Y = %d \n\n", xPos, yPos);

  for(i = 0; i < 100; i++) {

    // Random movement in X and Y (possibly 0)

    xPos = xPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];
    yPos = yPos + [uniformIntRand getIntegerWithMin: -1 withMax: 1];

    // Take move modulo maximum coordinate values

    xPos = (xPos+ worldXSize) % worldXSize;
    yPos = (yPos + worldYSize) % worldYSize;

    printf( "I moved to X = %d Y = %d \n", xPos, yPos);

  }

  return 0;

}
 
 




