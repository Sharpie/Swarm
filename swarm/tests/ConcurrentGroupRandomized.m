/*
Name:         ConcurrentGroupRandomized.m
Description:  ConcurrentGroup with Randomized order of execution.
Test suite:   activity
*/ 



#import "ConcurrentGroup_test.h"
#import "Responder.h"
#import <simtools.h>

int 
main (int argc, const char ** argv)
{
  id concGroupTest;
  id obj;
  int i, ok;
  
  initSwarm (argc, argv);
  init_tables ();
  
  concGroupTest = [ConcurrentGroup_test createBegin: globalZone 
					numberOfObjects: 5];
  for (i = 0; i < 5; i++)
    {
      obj = [Responder create:globalZone];
      [concGroupTest addObject: obj];
    }
  [concGroupTest setDefaultOrder: Randomized];
  concGroupTest = [concGroupTest createEnd];
  
  [concGroupTest createActionTo: [concGroupTest getObjectAt: 0]
                 message: M(m1)];
  [concGroupTest createActionTo: [concGroupTest getObjectAt: 1]
		 message: M(m2)];
  [concGroupTest createActionTo: [concGroupTest getObjectAt: 2]
		 message: M(m3)];	
  [concGroupTest createActionTo: [concGroupTest getObjectAt: 3]
		 message: M(m4)]; 
  [concGroupTest createActionTo: [concGroupTest getObjectAt: 3]
		 message: M(m5)]; 
  [[concGroupTest activateIn: nil] run];
  
  ok = 0;
  for (i = 0; i < 5; i++) 
    {
      if (!messages[i]) 
	{
	  fprintf (stderr, "Error in ActionGroupRandomized  method m%d not called !\n", i + 1);
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

