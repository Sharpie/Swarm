/*
Name:         DSSwarm.m
Description:  DynamicScheduleSwarm
Test suite:   activity
*/

#import "DSSwarm.h"
#import <activity.h>
#import <activity/Schedule.h>
#import <defobj/defalloc.h>

int ok=0;

@implementation DSSwarm

- buildActions
{
  firstSchedule = [Schedule createBegin: getZone(self)];
  [firstSchedule setAutoDrop: 1];
  firstSchedule = [firstSchedule createEnd];
  
  secondSchedule = [Schedule createBegin: getZone(self)];
  [secondSchedule setAutoDrop: 1];
  secondSchedule = [secondSchedule createEnd];
  
  [firstSchedule at: 10 createActionTo: self message: M(empty)];
  [secondSchedule at: 0 createActionTo: self message: 
		    M(schedulePrintOkOnFirstSchedule)];  
  return self;
}

- activateIn: swarmContext 
{
  [super activateIn: swarmContext];
  [firstSchedule activateIn: self];
  [secondSchedule activateIn: self];
  return self;
}

- schedulePrintOkOnFirstSchedule
{
  [firstSchedule at: (timeval_t) 5 createActionTo: self 
		 message: M(printOk)];
  return self;
}

- empty
{
  return self;
}

- printOk
{
  printf("Ok!\n");
  ok = 1;
  fflush(stdout);
  return self;
}

@end
