/*
Name:         ConcurrentGroup_test.h
Description:  ConcurrentGroup_test is a subclass of ConcurrentGroup, it is 
              used for testing ConcurrentGroup, but also subclasses
	      of it. 
Test suite:   activity
*/


#import <activity/ActionGroup.h>

@interface ConcurrentGroup_test_c: ConcurrentGroup_c
{
  id *objects;         // storage for objects that are recieving action
		       // from ConcurrentGroup
  int counter;
  int numberOfObjects; // number of objects in the object storage;

}

#include "Holder.h"

- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;

@end


