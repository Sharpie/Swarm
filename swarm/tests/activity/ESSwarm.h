/*
Name:         ESSwarm.h
Description:  ESSwarm (short for: EmptyScheduleSwarm) is used for
              testing of setKeepEmpty functionality.
Test suite:   activity
*/

#import <objectbase/Swarm.h>

extern int ok;

@interface ESSwarm: Swarm
{
  id firstSchedule;
  id secondSchedule;
}

- schedulePrintOkOnFirstSchedule;
- empty;
- printOk;
@end
