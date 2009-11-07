// Swarm library. Copyright � 1996-2000 Swarm Development Group.
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

#import <Swarm/space.h> // Discrete2d
#import <Swarm/openstep.h>
#import <Swarm/SwarmObject.h>
#import <Cocoa/Cocoa.h>

// generic object to handle display 2d objects
// hand it a 2d raster widget, tell it what message to send, and it sends it.
// also knows how to construct probes.

@interface Object2dDisplay: SwarmObject <Object2dDisplay>
{
  id <GridData> discrete2d;
  NSInvocation *displayInvocation;

  NSImage *image;
  NSBitmapImageRep *imageRep;
}

+ create: aZone withDiscrete2dToDisplay: (id <GridData>)c;
+ create: aZone withDiscrete2dToDisplay: (id <GridData>)c andDisplayInvocation: (NSInvocation *)anInvocation;

- setDiscrete2dToDisplay: (id <GridData>)c;
- setDisplayInvocation: (NSInvocation *)anInvocation;
- createEnd;

- (NSImage *)image;
- (id <GridData>)discrete2d;
- (void)updateDisplay;
- (void)makeProbeAtX: (unsigned)x Y: (unsigned)y;
@end
