/*
Name:         ActionGroupRandomized.m
Description:  ActionGroup with Randomized order of execution.
Test suite:   activity
*/ 

#import "ActionGroup_test.h"
#import "Responder.h"
#import <simtools.h>


int 
main (int argc, const char ** argv)
{
  id actionGroupTest;
  id obj;
  int i, ok;

  initSwarm (argc, argv);
  init_tables ();

  actionGroupTest = [ActionGroup_test createBegin: globalZone 
				      numberOfObjects: 5];
  for (i = 0; i < 5; i++)
    {
      obj = [Responder create:globalZone];
      [actionGroupTest addObject: obj];
    }
  [actionGroupTest setDefaultOrder: Randomized];
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
  
  [[actionGroupTest activateIn: nil] run];
  
  ok = 0;
  for (i = 0; i < 5; i++) 
    {
      if (!messages[i]) 
	{
	  fprintf (stderr,"Error in ActionGroupRandomized  method m%d not called !\n", i + 1);
	  return 1;
	}
      if (messages[i] != i + 1) 
	ok = 1;
    }
  
  if (ok)
    return 0;
  else 
    {
      fprintf (stderr, "Error ActionGroupRandomized should be randomized!\n");
      return 1;
    }
}

