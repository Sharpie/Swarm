/*
Name:         DynamicSchedule.h
Description:  DynamicSchedule tests for presence of dynamic scheduling bug, 
              which used to occur when an action is added to a Schedule 
	      from another Schedule, to be performed after the current time, 
	      but before first pending action in the schedule. Due to the 
	      bug action was never performed.
Test suite:   activity
*/


#import <simtools.h>
#import "DSSwarm.h"

int main(int argc, const char ** argv) 
{
  id theSwarm;

  initSwarm(argc, argv);
  theSwarm = [DSSwarm create: globalZone];
  [theSwarm buildActions];
  [theSwarm activateIn: nil];
  [[theSwarm getActivity] run];
  
  if (!ok)
    {
      fprintf(stderr, "Error in Schedule, dynamic update failed, action added after the current time, but before first pending action in the Schedule not performed!\n");
      return 1;
    }
  return 0;
}
