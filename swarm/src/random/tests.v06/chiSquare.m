// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Test the Chi square of a particular generator (via a Uniform distribution)
// This tests whether the distribution of numbers is roughly (but not
//   exactly) uniform.
// References: Sedgewick Algorithms, p. 517, and Knuth vol 2, page 40.

#import "genSelect.h"
#import "Uniform.h"
#import <stdio.h>
#import <math.h>

double chiSquare(Uniform *, int, int);

double
chiSquare(Uniform * u, int numBins, int numIter) {
  int i, *histo;
  double cs;

  histo = (int *) malloc(numBins * sizeof(*histo));
  for (i = 0; i < numBins; i++)
    histo[i] = 0;

  for (i = 0; i < numIter; i++) {
    unsigned r = [u rMax: numBins];
    histo[r]++;
  }

  cs = 0;
  for (i = 0; i < numBins; i++) {
    double v;
#ifdef PRINTHISTO
    printf("%10d %10d\n", i, histo[i]);
#endif
    cs += histo[i]*histo[i];
  }
  return ((cs * numBins) / (double)numIter) - numIter;
}

void
usage(char * s) {
  fprintf(stderr, "Usage: %s numBins [generatorType] [numIter]\n", s);
  dumpGeneratorList();
  exit(1);
}

int
main(int argc, char ** argv) {
  id generator;
  Uniform * u;

  int i;
  int generatorType, numBins, numIter;
  unsigned ok;

  double cs;

  initGeneratorArray();
  
  switch(argc) {
    case 1:  usage(argv[0]);
    case 2:  numBins = atoi(argv[1]); numIter = 20 * numBins; generatorType = 0; break;
    case 3:  numBins = atoi(argv[1]); numIter = 20 * numBins; generatorType = atoi(argv[2]); break;
    default: numBins = atoi(argv[1]); numIter = atoi(argv[3]); generatorType = atoi(argv[2]); break;
  }
  
  generator = [generators[generatorType] alloc];
  [generator init];				  // use a random seed

  u = [[[Uniform alloc] init] setGenerator: generator];

  printf("Doing chi-square for generator %s. %d bins, %d trials\n", [generator name], numBins, numIter);

  cs = chiSquare(u, numBins, numIter);
  printf("  Chi Square is %6g. Ideal is about %d.\n", cs, numBins);
  printf("  Difference is %6g, should be roughly within %6g\n",
	 fabs(cs - numBins), 2*sqrt(numBins));

  ok = (fabs(cs - numBins) < 2*sqrt(numBins));
  
  printf("Generator is %s. Run this several times: should be ok only 90%% times.\n",
	 ok ? "ok" : "suspicious");

  return ok ? 0 : 1;
}
