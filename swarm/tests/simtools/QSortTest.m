// Swarm library. Copyright (C) 1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>
#import <objectbase.h>
#import <collections.h>

#include <misc.h>
#include <swarmconfig.h> // PTRINT

int
main (int argc, const char **argv) 
{
  id list;
  id index;
  id member;
  int currMax, currMin;

  initSwarmBatch (argc, argv);

  // first, create a collection to be ordererd....
  list = [List create: globalZone];

  [list addLast: (id) 3];
  [list addLast: (id) 10];
  [list addLast: (id) 1];
  [list addLast: (id) 12];
  [list addLast: (id) 13];
  [list addLast: (id) 389];
  [list addLast: (id) 99];

  // check list in unsorted order
  printf ("unsorted list...\n");
  index = [list begin: scratchZone];
  
  while ((member = [index next])) 
    printf (PTRINTFMT ", ", (PTRINT) member);
  [index drop];

  // reverse the list...
  [QSort reverseOrderOf: list];

  // check reversed order
  printf ("\nreversed list...\n");
  index = [list begin: scratchZone];
  while ((member = [index next])) 
    printf (PTRINTFMT ", ", (PTRINT) member);
  [index drop];

  // sort the list...
  [QSort sortNumbersIn: list];

  // check list in sorted order
  currMax = 0;
  printf ("\nsorted (ascending) list...\n");
  index = [list begin: scratchZone];
  while ((member = [index next])) 
    {
      if (currMax > (PTRINT) member)
        {
          fprintf(stderr, "list is not sorted in ascending order\n");
          return 1;
        }
      currMax = (PTRINT) member;
      printf (PTRINTFMT ", ", (PTRINT) member);
    }
  [index drop];

  // now reverse list...
  currMin = 999;  
  [QSort reverseOrderOf: list];
  
  printf("\nsorted and reversed list (descending)...\n");
  index = [list begin: scratchZone];
  while ((member = [index next])) 
    {
      if (currMin < (PTRINT) member)
        {
          fprintf (stderr, "list is not sorted in descending order\n");
          return 1;
        }
      currMin = (PTRINT) member;
      printf (PTRINTFMT ", ", (PTRINT) member);
    }
  [index drop];

  printf ("\n");

  return 0;
}


