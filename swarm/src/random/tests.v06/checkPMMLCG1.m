// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Quick code to test the 10000th output of PMMLCG1 against a known value.
// Checks if the math is handling overflow type things right.

#import <stdio.h>
#import <defobj.h>
#import <random/PMMLCGgen.h> 

int
main(int argc, char ** argv) {
  id generator;
  unsigned i;
  unsigned bitbucket;

  generator = [PMMLCG1gen create: [self getZone] setStateFromSeed: 1];

  for (i = 0; i < 9999; i++)
    bitbucket = [generator getUnsignedSample];

  if ([generator getUnsignedSample] == 1043618064) {
    printf("10000th PMMLCG1 checked out ok: math is probably right\n");
    return 0;
  } else {
    printf("10000th PMMLCG1 failed. Probably a math error.\n");
    return 1;
  }
}
