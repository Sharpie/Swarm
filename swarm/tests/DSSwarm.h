/*
Name:         DSSwarm.h
Description:  DSSwarm (short for: DynamicScheduleSwarm) is used for
              testing for presence of dynamic scheduling bug, which used
	      to occur when an action is added to a Schedule from another
	      Schedule, to be performed after the current time, but before 
	      first pending action in the schedule.  Due to the bug action 
	      was never performed.
Test suite:   activity
*/

#import <objectbase/Swarm.h>

extern int ok;

@interface DSSwarm: Swarm
{
  id firstSchedule;
  id secondSchedule;
}

- schedulePrintOkOnFirstSchedule;
- printTime;
- printOk;
@end
