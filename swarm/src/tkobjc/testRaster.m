// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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
