/*
Name:         ActionGroup_test.m
Description:  ActionGroup subclass implementation
Test suite:   activity
*/ 


#import "ActionGroup_test.h"
#import <defobj.h>

@implementation ActionGroup_test

#define CLASS_NAME ActionGroup_test
#include "Holder.m"

- (void) describe: (id) outputCharStream
{
  char description[100];

  sprintf(description, "Randomized:        %d\n", getBit(bits,BitRandomized));
  sprintf(description, "Number of objects: %d\n", numberOfObjects); 
  [outputCharStream catC: description];
}

- (void) describeForEach: (id) outputCharStream
{
  char buffer[50];
  int i;
  for (i=0;i<numberOfObjects;i++)
    {
      id obj = *(objects+i);
      if (respondsTo(obj, M(describe:)))
	[obj describe: outputCharStream];
      else
	{
	  _obj_formatIDString( buffer, obj);
	  [outputCharStream catC: buffer];
	}
    }
}

@end
