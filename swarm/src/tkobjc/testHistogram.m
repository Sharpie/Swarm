// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdio.h>
#import <stdlib.h>
#import <tkobjc.h>
#import <collections.h>

int valueOne;
char *valueTwo;

int
main(int argc, char **argv)
{
  Histogram *h;
  double data[] = { 1.0, 3.0, 2.0, 5.0, 0.0, 6.0, 2.0, 9.0, 1.0, 4.0};
  const char *names[] = 
  {"foo", "bar", "baz", "a", "b", "longname",
   "foo", "bar", "baz", "a"};
  const char *colors[] = 
  {"black", "grey50", "red", "green", "blue", "orange", "purple", "yellow",
   "black", "grey50", "red", "green", "blue", "orange", "purple", "yellow"};
  
  initModule(collections);
  initTkObjc(argc, argv);
  
  h = [Histogram create: globalZone];
  [h setNumPoints: 10 Labels: names Colors: colors];
  [h drawHistogramWithDouble: data];
  [h pack];
  
  [globalTkInterp promptAndEval];
  exit (0);
}
