/*
Name:         ConcurrentGroup_test.m
Description:  ConcurrentGroup subclass implementation
Test suite:   activity
*/ 


#import "ConcurrentGroup_test.h"
#import <defobj.h>

@implementation ConcurrentGroup_test_c
#define CLASS_NAME ConcurrentGroup_test_c

PHASE(Creating)
#define MIXIN_CREATE
#include "Holder.m"

PHASE(Using)
#undef MIXIN_CREATE
#include "Holder.m"

- (void)describe: outputCharStream
{
  char description[100];
  
  sprintf (description, "Randomized:        %d\n", 
	   getBit (bits, BitRandomized));
  sprintf (description, "Number of objects: %d\n", numberOfObjects); 
  [outputCharStream catC: description];
}

- (void)describeForEach: outputCharStream
{
  char buffer[50];
  int i;
  for (i = 0; i < numberOfObjects; i++)
    {
      id obj = *(objects + i);
      if (respondsTo (obj, M(describe:)))
   	  [obj describe: outputCharStream];
      else
	{
	  _obj_formatIDString (buffer, obj);
	  [outputCharStream catC: buffer];
	}
    }
}

@end
