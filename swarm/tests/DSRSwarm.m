/*
Name:         DSRSwarm.m
Description:  DynamicScheduleRepeatSwarm implementation
Test suite:   activity
*/

#import "DSRSwarm.h"
#import <activity.h>
#import <activity/Schedule.h>
#import <defobj/defalloc.h>

int stimes[10] = { 0,0,0,0,0,
		  0,0,0,0,0 };
int timer = 0;

@implementation DSRSwarm
- buildActions
{
  firstSchedule = [Schedule createBegin: getZone(self)];
  [firstSchedule setRepeatInterval: 10];
  firstSchedule = [firstSchedule createEnd];
  
  secondSchedule = [Schedule createBegin: getZone(self)];
  secondSchedule = [secondSchedule createEnd];

  [firstSchedule at: 3 createActionTo: self message: M(empty)];
  [secondSchedule at: 0 createActionTo: self message: 
		    M(schedulePrintOkOnFirstScheduleAfterCurrentTime)];
  [secondSchedule at: 6 createActionTo: self message: 
		    M(schedulePrintOkOnFirstScheduleBeforeCurrentTime)];
  [secondSchedule at: 30 createActionTo: self message:
		    M(stopRunning)];
  return self;
}

- activateIn: swarmContext 
{
  [super activateIn: swarmContext];
  [firstSchedule activateIn: self];
  [secondSchedule activateIn: self];
  return self;
}

- schedulePrintOkOnFirstScheduleAfterCurrentTime
{
  [firstSchedule at: (timeval_t) getCurrentTime() + 1 createActionTo: self 
		 message: M(printOk)];
  return self;
}

- schedulePrintOkOnFirstScheduleBeforeCurrentTime
{
  [firstSchedule at: (timeval_t) getCurrentTime() - 1 createActionTo: self 
		 message: M(printOk)];
  return self;
}
- empty
{
  return self;
}

- printOk
{
  printf("%d: Ok!\n", (int) getCurrentTime() );
  fflush(stdout);
  stimes[timer++] = (int) getCurrentTime();
  return self;
}

- stopRunning
{
  [getTopLevelActivity() terminate];
  return self;
}

