// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Quick code to print out bit patterns from generator
// You can inspect this by eye if you think it will help you.

#import "genSelect.h"
#import <stdio.h>
#import "LCG.h"

void printBits(unsigned);

void
printBits(unsigned n) {
  int i;
  for (i = 31; i >= 0; i--) {
    putchar(n & (1U << i) ? '1' : '0');
    putchar(' ');
  }
}

void
usage(char * s) {
  fprintf(stderr, "Usage: %s numIter [generatorType] [seed]\n", s);
  dumpGeneratorList();
  exit(1);
}

int
main(int argc, char ** argv) {
  id generator;
  unsigned generatorType;
  unsigned numIter;
  unsigned i;
  unsigned seed = 0, seedGiven = 0;

  initGeneratorArray();
  
  switch(argc) {
    case 1:  usage(argv[0]);
    case 2:  numIter = atoi(argv[1]); generatorType = 0; break;
    case 3:  numIter = atoi(argv[1]); generatorType = atoi(argv[2]); break;
    default: numIter = atoi(argv[1]); generatorType = atoi(argv[2]); seed = atoi(argv[3]); seedGiven = 1; break;
  }
  
  generator = [generators[generatorType] alloc];
  if (seedGiven)
    [generator initSeed: seed];
  else
    [generator init];				  // use a random seed
  printf("Printing %d outputs from generator %s\n", numIter, [generator name]);
  if ([generator isKindOf: [LCG self]])
    printf("  LCGs with modulus 2^32 are known to have problems. Look at the low bits.\n");

  for (i = 0; i < numIter; i++) {
    unsigned r = [generator r];
    printf("%10u  ", r);
    printBits(r);
    putchar('\n');
  }

  return 0;
}
