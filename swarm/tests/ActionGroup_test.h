/*
Name:         ActionGroup_test.h
Description:  ActionGroup_test is a subclass of ActionGroup, it is 
              used for testing ActionGroup, but also subclasses
	      of it. 
Test suite:   activity
*/



#import <activity/ActionGroup.h>

@interface ActionGroup_test: ActionGroup_c
{
  id *objects;         // storage for objects that are recieving action
		       // from ActionGroup
  int counter;
  int numberOfObjects; // number of objects in the object storage;

}

#include "Holder.h"

- (void) describe: (id) outputCharStream;
- (void) describeForEach: outputCharStream;

@end


