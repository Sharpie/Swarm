// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Test whether random bits are doing well: print them out, as well
// as run lengths.

#import "RandomBit.h"
#import "genSelect.h"
#import <stdio.h>

#define MAXRUNLENGTH 100

void
usage(char * s) {
  fprintf(stderr, "Usage: %s numIter [generatorType]\n", s);
  dumpGeneratorList();
  exit(1);
}

int
main(int argc, char ** argv) {
  id generator;
  RandomBit * rb;
  unsigned generatorType;
  unsigned numIter;
  unsigned i;
  unsigned lastBit, thisRunLength, maxLengthEncountered;
  unsigned runLengths[MAXRUNLENGTH];

  initGeneratorArray();
  
  switch(argc) {
    case 1:  usage(argv[0]);
    case 2:  numIter = atoi(argv[1]); generatorType = 0; break;
    default: numIter = atoi(argv[1]); generatorType = atoi(argv[2]); break;
  }
  
  generator = [generators[generatorType] alloc];
  [generator init];				  // use a random seed
  rb = [[[RandomBit alloc] init] setGenerator: generator];

  printf("Printing %d bits from generator %s\n", numIter, [generator name]);

  // prime the loop
  for (i = 0; i < MAXRUNLENGTH; i++)
    runLengths[i] = 0;
  thisRunLength = 0;
  maxLengthEncountered = 0;

  lastBit = [rb bit];
  putchar(lastBit+'0');
  
  for (i = 1; i < numIter; i++) {
    unsigned bit = [rb bit];
    putchar(bit + '0');

    if (bit == lastBit)
      thisRunLength++;
    else {
      if (thisRunLength > maxLengthEncountered)
	maxLengthEncountered = thisRunLength;
      if (thisRunLength >= MAXRUNLENGTH) {
	fprintf(stderr, "Run length exceeded MAXRUNLENGTH. Binning at top.\n");
	thisRunLength = MAXRUNLENGTH-1;
      }
      runLengths[thisRunLength]++;		  // thisRunLength actually
      lastBit = bit;				  // is length - 1.
      thisRunLength = 0;
    }
  }
#ifdef COUNTLAST
  // handle the last value: probably not actually a good idea to do so.

  if (thisRunLength > maxLengthEncountered)
    maxLengthEncountered = thisRunLength;
  if (thisRunLength >= MAXRUNLENGTH) {
    fprintf(stderr, "Run length exceeded MAXRUNLENGTH. Binning at top.\n");
    thisRunLength = MAXRUNLENGTH-1;
  }
  runLengths[thisRunLength]++;
#endif

  putchar('\n');

  printf("Now printing run lengths.\nLength  Number  Fraction\n");
  for (i = 0; i < maxLengthEncountered + 1; i++)
    printf("%6d  %6d  %.6g\n",
	   i+1, runLengths[i], (double) runLengths[i] / (double) numIter);
  return 0;
}
