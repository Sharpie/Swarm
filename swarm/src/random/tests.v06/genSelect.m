// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// generic code to implement selection of generators for testing.

#import <objc/Object.h>
#import "PMMLCG.h"
#import "LCG.h"
#import "SWB.h"
#import "ACG.h"
#import "SCG.h"

#define NUMGENERATORS 10
Class generators[NUMGENERATORS];
void
initGeneratorArray(void) {
  generators[0] = [LCG1 self];
  generators[1] = [LCG2 self];
  generators[2] = [PMMLCG1 self];
  generators[3] = [PMMLCG2 self];
  generators[4] = [PMMLCG3 self];
  generators[5] = [SWB1 self];
  generators[6] = [SWB2 self];
  generators[7] = [SWB3 self];
  generators[8] = [ACG1 self];
  generators[9] = [SCG1 self];
}

void
dumpGeneratorList(void) {
  int i;
  for (i = 0; i < NUMGENERATORS; i++)
    fprintf(stderr, "  %2d %s\n", i, [generators[i] name]);
}
