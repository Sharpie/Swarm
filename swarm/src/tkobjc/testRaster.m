// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "tkobjc.h"

id globalTkInterp;

int
main(int argc, char ** argv) {
  XColormap * comap;
  ZoomRaster * displayWindow;
  unsigned i, j;

  initCollections();
  initTkObjc(argc, argv);
  
  comap = [XColormap create: globalZone];
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
}
