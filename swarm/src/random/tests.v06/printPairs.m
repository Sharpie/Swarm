// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Quick code to print out bit patterns from generator
// You can inspect this by eye if you think it will help you.

#import "genSelect.h"
#import "Uniform.h"
#import <stdio.h>

void
usage(char * s) {
  fprintf(stderr, "Usage: %s numIter [generatorType]\n", s);
  dumpGeneratorList();
  exit(1);
}

int
main(int argc, char ** argv) {
  id generator;
  unsigned generatorType;
  unsigned numIter;
  unsigned i;
  float last;
  Uniform * u;

  initGeneratorArray();
  
  switch(argc) {
    case 1:  usage(argv[0]);
    case 2:  numIter = atoi(argv[1]); generatorType = 0; break;
    default: numIter = atoi(argv[1]); generatorType = atoi(argv[2]); break;
  }
  
  generator = [generators[generatorType] alloc];
  [generator init];				  // use a random seed
  u = [[[Uniform alloc] init] setGenerator: generator];

  last = [u rFloat];
  for (i = 0; i < numIter; i++) {
    printf("%6f ", last);
    last = [u rFloat];
    printf("%6f ", last);
    putchar('\n');
  }

  return 0;
}
