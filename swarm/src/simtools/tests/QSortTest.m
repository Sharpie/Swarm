// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdio.h>
#import <stdlib.h>
#import <swarmobject.h>
#import <simtools.h>
#import <collections.h>

int
main(int argc, char ** argv) {
  id  theList;
  id index;
  id member;

  initSwarm(argc, argv);

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

  printf("unsorted list...\n");
  index = [theList begin: globalZone];
  while ( (member = [index next]) ) {
    printf("member = %d\n", (int) member);
  }
  [index drop];

  // reverse the list...
  [QSort reverseOrderOf: theList];

  // check reversed order

  printf("\nreversed list...\n");
  index = [theList begin: globalZone];
  while ( (member = [index next]) ) {
    printf("member = %d\n", (int) member);
  }
  [index drop];

  // sort the list...
  
  [QSort sortNumbersIn: theList];

  // check list in sorted order

  printf("\nsorted list...\n");
  index = [theList begin: globalZone];
  while ( (member = [index next]) ) {
    printf("member = %d\n", (int) member);
  }
  [index drop];

  // now reverse list...
  
  [QSort reverseOrderOf: theList];
  
  printf("\nsorted and reversed list...\n");
  index = [theList begin: globalZone];
  while ( (member = [index next]) ) {
    printf("member = %d\n", (int) member);
  }
  [index drop];

  return 0;
}


