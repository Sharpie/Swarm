/*
Name:         Schedule.m
Description:  Simple test of Schedule. Tests scheduling of concurrent actions.
Test suite:   activity
*/


#import "Responder.h"
#import <simtools.h>
#import <collections/Array.h>
#import <activity/Schedule.h>

int 
main (int argc, const char ** argv)
{
  id array;
  id obj;
  int i;
  id schedule;
  
  initSwarm (argc, argv);
  init_tables ();

  array = [Array create: globalZone setCount: 5];
  for (i = 0; i < 5; i++)
    {
      obj = [Responder create:globalZone];
      [obj setId: i + 1];
      [array atOffset: i put: obj];
    }

  schedule = [Schedule createBegin: globalZone];
  [schedule setAutoDrop: 1];
  schedule = [schedule createEnd];
  
  [schedule at: 0 createActionTo: [array atOffset: 0]
	    message: M(mTimeId)];
  [schedule at: 1 createActionTo: [array atOffset: 0]
	    message: M(mTimeId)];
  [schedule at: 1 createActionTo: [array atOffset: 1]
	    message: M(mTimeId)];
  [schedule at: 10 createActionTo: [array atOffset: 2]
	    message: M(mTimeId)];	
  [schedule at: 10 createActionTo: [array atOffset: 3]
	    message: M(mTimeId)]; 
  [schedule at: 100 createActionTo: [array atOffset: 4]
	    message: M(mTimeId)];
  [schedule at: 100 createActionTo: [array atOffset: 4]
	    message: M(mTimeId)];
  [[schedule activateIn: nil] run];
  
  if (ids[0] != 1)
    {
      fprintf (stderr, "Error in Schedule, action scheduled for time: 0 not performed! \n");
      return 1;
    }
  if (ids[1] != 1 || ids[2] != 2 || rtimes[1] != 1 || rtimes[2] != 1)
    {
      fprintf (stderr, "Error in Schedule, two concurrent actions scheduled for time: 1 not performed! \n ");
      return 1;
    }
  if (ids[3] != 3 || ids[4] != 4 || rtimes[3] != 10 || rtimes[4] != 10)
    {
      fprintf (stderr, "Error in Schedule, two concurrent actions scheduled for time: 10 not performed! \n ");
      return 1;
    }
  if (ids[5] != 5 || ids[6] != 5 || rtimes[5] != 100 || rtimes[6] != 100)
    {
      fprintf (stderr, "Error in Schedule, two concurrent actions scheduled for time: 100 not performed! \n");
    }
  return 0;
}





