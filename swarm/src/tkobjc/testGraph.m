// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h>
#import <gui.h>
#import <defobj.h>

int
main (int argc, const char **argv)
{
  id <Graph> g;
  id <GraphElement> e;
  unsigned i;
  double data[] = { 1.0, 3.0, 2.0, 5.0, 0.0, 6.0, 2.0, 9.0, 1.0, 4.0};

  initSwarm (argc, argv);
  
  g = [Graph create: globalZone];
  [g setTitle: "Test of Graph"];
  [g setAxisLabelsX: "x" Y: "data"];
  e = [g createElement];
  [e setLabel: "testdata"];
  [e setColor: "black"];
  [e setWidth: 0];
  [e setSymbol: "diamond"];
  [e setSymbolSize: 1];
  [e setDashes: 0];
  for (i = 0; i < 10; i++)
    [e addX: i Y: data[i]];
  
  [g pack];

  while (1)
    {
      while (GUI_EVENT_ASYNC ());
      [e addX: i Y: random() % 10 ];
      if (i % 100 == 0)
        [e resetData];
      i++;
    }
  return 0;
}

/*
Local Variables:
compile-command: "$SWARMHOME/bin/libtool-swarm --mode=link /opt/SDGgcc/2.95.2/bin/gcc -o testGraph -g -Wno-import -I$SWARMHOME/include/swarm -L$SWARMHOME/lib/swarm testGraph.m -lswarm -lobjc"
End:
*/
