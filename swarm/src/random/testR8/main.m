// HumBug: a dummy application for testing
// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "main.m"

#import <simtools.h>
#import "HumbugBatchSwarm.h"

int
main(int argc, char ** argv) {

  HumbugBatchSwarm * batchSwarm;

  initSwarm(argc, argv);

    batchSwarm = [HumbugBatchSwarm create: globalZone];
    [batchSwarm buildObjects];
    [batchSwarm buildActions];
    [batchSwarm activateIn: nil];
    [batchSwarm go];

  return 0;
}
