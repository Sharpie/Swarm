/*
Name:         DSRSwarm.h
Description:  DSRSwarm (short for: DynamicScheduleRepeatSwarm) is used for
              testing for presence of dynamic scheduling bug in schedule with
	      repeat cycle. 
Test suite:   activity
*/

#import <objectbase/Swarm.h>

extern int stimes[10];

@interface DSRSwarm: Swarm
{
  id firstSchedule;
  id secondSchedule;
}

- printTime;
- printOk;
- stopRunning;
- schedulePrintOkOnFirstScheduleAfterCurrentTime;
- schedulePrintOkOnFirstScheduleBeforeCurrentTime;

@end
