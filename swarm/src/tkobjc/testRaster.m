// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>
#import <objectbase/Arguments.h>

id globalTkInterp;

int
main (int argc, const char **argv)
{
  Colormap *comap;
  ZoomRaster *displayWindow;
  unsigned i, j;

  initModule (collections);
  initTkObjc ([Arguments createArgc: argc Argv: argv]);
  
  comap = [Colormap create: globalZone];
  [comap setColor: 1 ToName: "red"];
  [comap setColor: 2 ToName: "blue"];

  displayWindow = [ZoomRaster create: globalZone];
  [displayWindow setColormap: comap];
  [displayWindow setZoomFactor: 8];
  [displayWindow setWidth: 10 Height: 10];
  [displayWindow pack];

  for (i = 0; i < 10; i++)
    for (j = 0; j < 10; j++)
      [displayWindow drawPointX: i Y: j Color: (i + j) % 2 + 1];
  [displayWindow drawSelf];

  [globalTkInterp promptAndEval];
  exit (0);
}
