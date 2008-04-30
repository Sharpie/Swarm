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

#import <swarmgstep.h>
#import <objectbase/SwarmObject.h>
#import <AppKit/AppKit.h>

// generic object to handle display 2d values

@interface Value2dDisplay: SwarmObject <Value2dDisplay>
{
#ifndef GNUSTEP
  id <Raster> displayWidget;
#else
  id displayWidget;
#endif
  IMP drawPointImp;
#ifndef GNUSTEP
  id <Colormap> colormap;
#else
  id colormap;
#endif
  id <GridData> discrete2d;  // read only
  int numCaStates;
  int modFactor;
  int colorConstant;
}
#ifndef GNUSTEP
+ create: aZone setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c setDiscrete2dToDisplay: (id <GridData>)d;
- setDisplayWidget: (id <Raster>)r colormap: (id <Colormap>)c;
#else
+ create: aZone setDisplayWidget: (id)r colormap: (id)c setDiscrete2dToDisplay: (id <GridData>)d;
- setDisplayWidget: (id)r colormap: (id)c;
- displayX:(int)xPos Y:(int)yPos inRect:(NSRect)aRect;
#endif
- setDiscrete2dToDisplay: (id <GridData>)c;
- (id <GridData>)discrete2d;
- setDisplayMappingM: (int)m C: (int)c;	  // linear mapping
- display;
@end

