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
main(int argc, const char ** argv) 
{
  id theList;
  id index;
  id member;
  int currMax, currMin;

  initSwarmBatch(argc, argv);

  // first, create a collection to be ordererd....
  theList = [List createBegin: globalZone];
  theList = [theList createEnd];

  [theList addLast: (id) 3];
  [theList addLast: (id) 10];
  [theList addLast: (id) 1];
  [theList addLast: (id) 12];
  [theList addLast: (id) 13];
  [theList addLast: (id) 389];
  [theList addLast: (id) 99];

  // check list in unsorted order
  printf ("unsorted list...\n");
  index = [theList begin: globalZone];
  
  while ((member = [index next])) 
    printf (PTRINTFMT ", ", (PTRINT) member);
  [index drop];

  // reverse the list...
  [QSort reverseOrderOf: theList];

  // check reversed order
  printf ("\nreversed list...\n");
  index = [theList begin: globalZone];
  while ((member = [index next])) 
    printf (PTRINTFMT ", ", (PTRINT) member);
  [index drop];

  // sort the list...
  [QSort sortNumbersIn: theList];

  // check list in sorted order
  currMax = 0;
  printf ("\nsorted (ascending) list...\n");
  index = [theList begin: globalZone];
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
  [QSort reverseOrderOf: theList];
  
  printf("\nsorted and reversed list (descending)...\n");
  index = [theList begin: globalZone];
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


