// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Quick code to test the 11th output of LCG1 against a known value.
//   (published in Numerical Recipes)
// Checks if the math is handling overflow type things right.

#import "LCG.h"
#import <stdio.h>

int
main(int argc, char ** argv) {
  id generator;
  unsigned i;

  generator = [LCG1 alloc];
  [generator initSeed: 0];

  for (i = 0; i < 10; i++)
    [generator r];

  if ([generator r] == 3421909937U) {
    printf("11th LCG1 checked out ok: math is probably right\n");
    return 0;
  } else {
    printf("11th LCG1 failed. Probably a math error.\n");
    return 1;
  }
}
