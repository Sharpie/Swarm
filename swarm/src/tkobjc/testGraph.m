// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdio.h>
#import <stdlib.h>
#import <tkobjc.h>
#import <collections.h>
#import <objectbase/Arguments.h>

int
main (int argc, const char **argv)
{
  Graph *g;
  GraphElement *e;
  unsigned i;
  double data[] = { 1.0, 3.0, 2.0, 5.0, 0.0, 6.0, 2.0, 9.0, 1.0, 4.0};

  initModule (collections);
  initTkObjc ([Arguments createArgc: argc Argv: argv]);
  
  g = [Graph create: globalZone];
  [g setTitle: "Test of Graph"];
  [g setAxisLabelsX: "x" Y: "data"];
  e = [g createElement];
  [[[e setLabel: "testdata"] setColor: "black"] setWidth: 2];
  [e setDashes: 2];
  for (i = 0; i < 10; i++)
    [e addX: i Y: data[i]];
  
  [g pack];

#if defined(SIMPLELOOP) || 1
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
