// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdio.h>
#import <stdlib.h>
#import "tkobjc.h"
#import <collections.h>

int
main(int argc, char ** argv) {
  BLTGraph * g;
  GraphElement * e;
  unsigned i;
  double data[] = { 1.0, 3.0, 2.0, 5.0, 0.0, 6.0, 2.0, 9.0, 1.0, 4.0};

  initModule(collections);
  initTkObjc(argc, argv);
  
  g = [BLTGraph create: globalZone];
  [g title: "Test of BLTGraph"];
  [g axisLabelsX: "x" Y: "data"];
  e = [g createElement];
  [[[e setLabel: "testdata"] setColor: "black"] setWidth: 2];
  [e setDashes: 2];
  for (i = 0; i < 10; i++)
    [e addX: i Y: data[i]];
  
  [g pack];

#ifdef SIMPLELOOP
  [globalTkInterp promptAndEval];
#else
  while (tk_NumMainWindows > 0) {
    while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT))
      ;
    [e addX: i Y: random() % 10 ];
    if (i % 100 == 0)
      [e resetData];
    i++;
  }
#endif
  return 0;
}
