/*
Name:         ESSwarm.m
Description:  EmptyScheduleSwarm
Test suite:   activity
*/

#import "ESSwarm.h"
#import <activity.h>
#import <activity/Schedule.h>
#import <defobj/defalloc.h>

int ok = 0;

@implementation ESSwarm

- buildActions
{
  firstSchedule = [Schedule createBegin: getZone (self)];
  [firstSchedule setAutoDrop: YES];
  [firstSchedule setKeepEmptyFlag: YES];
  firstSchedule = [firstSchedule createEnd];
  
  secondSchedule = [Schedule createBegin: getZone (self)];
  [secondSchedule setAutoDrop: YES];
  secondSchedule = [secondSchedule createEnd];
  
  [firstSchedule at: 2 createActionTo: self message: M(empty)];
  [secondSchedule at: 3 createActionTo: self 
		  message: M(schedulePrintOkOnFirstSchedule)];  
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
  printf ("Ok!\n");
  ok = 1;
  fflush (stdout);
  return self;
}

@end






