/*
Name:         DynamicScheduleRepeat.m
Description:  DynamicScheduleRepeat tests for presence of dynamic scheduling 
              bug in schedule with repeat cycle. 
Test suite:   activity
*/

#import <simtools.h>
#import "DSRSwarm.h"

int main(int argc, const char ** argv) 
{
  id theSwarm;
  
  initSwarm(argc, argv);
  
  theSwarm = [DSRSwarm create: globalZone];
  
  [theSwarm buildActions];
  [theSwarm activateIn: nil];
  [[theSwarm getActivity] run];
  
  if (stimes[0] != 1)
    {
      fprintf(stderr,"Error dynamic update failed, action added after current\ntime but before first action in the schedule never was performed!\n ");
      return 1;
    }
  if (stimes[1] != 11)
    {
      fprintf(stderr, "Error dynamic update failed, action added in previous\nrepeat cycle never performed!\n");
      return 1;
    }
  if (stimes[2] != 15)
    {
      fprintf(stderr, "Error dynamic update failed, action added in previous repeat cycle before current time, never performed in last repeat cycle!\n" );
      return 1;
    }
  if (stimes[3] != 21)
    {
      fprintf(stderr,  "Error dynamic update failed, action added two repeat cycles ago, that was performed in previous cycle never performed in last repeat cycle!\n ");
      return 1;
    }
  if (stimes[4] != 25)
    {
      fprintf(stderr,  "Error dynamic update failed, action added before current time two repeat cycles ago, that was performed in previous cycle never performed in last repeat cycle\n ");
      return 1;
    }
  return 0;
}
