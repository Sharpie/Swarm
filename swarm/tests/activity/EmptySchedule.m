/*
Name:         EmptySchedule.m
Description:  EmptySchedule is used for testing of setKeepEmpty 
              functionality.
Test suite:   activity
*/


#import <simtools.h>
#import "ESSwarm.h"

int 
main (int argc, const char ** argv) 
{
  id theSwarm;

  initSwarmBatch (argc, argv);
  theSwarm = [ESSwarm create: globalZone];
  [theSwarm buildActions];
  [theSwarm activateIn: nil];
  [[theSwarm getActivity] run];
  
  if (!ok)
    {
      fprintf (stderr, "Error in Schedule, action added to an empty schedule never performed, although setKeepEmpty was set to YES!\n");
      return 1;
    }
  return 0;
}


