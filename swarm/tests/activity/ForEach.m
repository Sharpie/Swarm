/*
Name:         ForEach.m
Description:  Tests ForEachAction performed in Sequential order. It also
              performs a test of ActionGroup.
Test suite:   activity
*/


#import <simtools.h>
#import "tactivity.h"
#import "Responder.h"

int 
main (int argc, const char ** argv)
{
  id actionGroupTest;
  id obj;
  id collection;
  int i, ok;
  
  initSwarmBatch (argc, argv);
  initModule (tactivity);
  init_tables ();
  
  actionGroupTest = [ActionGroup_test createBegin: globalZone 
				      numberOfObjects: 6];
  for (i = 0; i < 5; i++)
    {
      obj = [Responder create:globalZone];
      [actionGroupTest addObject: obj];
    }
  collection = [List createBegin: globalZone];
  collection = [collection createEnd];
  for (i = 1; i <= 5; i++)
    {
      [collection addLast: [Responder create: globalZone withId: i]];
    }
  [actionGroupTest addObject: collection];
  
  [actionGroupTest setDefaultOrder: Sequential];
  actionGroupTest = [actionGroupTest createEnd];
  
  
  [actionGroupTest createActionTo: [actionGroupTest getObjectAt: 0]
		   message: M(m1)];
  [actionGroupTest createActionTo: [actionGroupTest getObjectAt: 1]
		   message: M(m2)];
  [actionGroupTest createActionTo: [actionGroupTest getObjectAt: 2]
		   message: M(m3)];	
  [actionGroupTest createActionTo: [actionGroupTest getObjectAt: 3]
		   message: M(m4)];	
  [actionGroupTest createActionTo: [actionGroupTest getObjectAt: 4]
		   message: M(m5)];	
  [actionGroupTest createActionForEach: [actionGroupTest getObjectAt: 5]
		   message: M(mId)];  
  [[actionGroupTest activateIn: nil] run];
  
  ok = 1;
  for (i = 0; i<5; i++) 
    {
      if (!messages[i]) 
	{
	  fprintf (stderr, "Error in ActionGroupRandomized  method m%d not called !\n", i + 1);
	  return 1;
	}
      if (!ids[i])
	{
	  fprintf (stderr, "Error in ForEachAction message not sent to member of \n collection at index %d !\n", i);
	  return 1;
	}
      if (ids[i] != i + 1) 
	ok = 0;
    }
  
  if (ok)
    return 0;
  else 
    {
      fprintf (stderr, "Error ForEachAction should not be randomized!\n");
      return 1;
    }
}



