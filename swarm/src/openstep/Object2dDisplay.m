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

// Loop through a Discrete2d, sending the displayMessage message to
// all objects found in there.  One argument is passed on the message,
// the display widget.

#import "Object2dDisplay.h"
#import <space/Discrete2d.h> // discrete2dSiteAt
#import <defobj.h> // ProtocolViolation

@implementation Object2dDisplay

PHASE(Creating)

+ create: aZone withDiscrete2dToDisplay: (id <GridData>)c
{
    return [self create: aZone withDiscrete2dToDisplay: c andDisplayInvocation: nil];
}

+ create: aZone withDiscrete2dToDisplay: (id <GridData>)c andDisplayInvocation: (NSInvocation *)anInvocation
{
    Object2dDisplay *obj = [self createBegin: aZone];
    
    [obj setDiscrete2dToDisplay: c];
    [obj setDisplayInvocation: anInvocation];
    
    return [obj createEnd];
}

- setDiscrete2dToDisplay: (id <GridData>)c
{
    discrete2d = c;
    
    return self;
}

- setDisplayInvocation: (NSInvocation *)s
{
    if (displayInvocation) [displayInvocation release];
    
    // create a copy as we will modify it when using it
    if (s) {
        displayInvocation = [NSInvocation invocationWithMethodSignature: [s methodSignature]];
        [displayInvocation setSelector: [s selector]];
    } else
        displayInvocation = nil;
    
    return self;
}

- createEnd
{
    [super createEnd];
    
    return self;
}

PHASE(Using)

- (NSImage *)image
{
    return image;
}

- (void)releaseImage
{
    if (image)
    {
        [image release];
        image = nil;
    }
    if (imageRep)
    {
        [imageRep release];
        imageRep = nil;
    }
}

- (void)createImage
{
    //NSRect aRect = [self bounds];
    
    [self releaseImage];
    
	float worldXsize = (float)[discrete2d getSizeX];
	float worldYsize = (float)[discrete2d getSizeY];
	NSRect aRect = NSMakeRect(0, 0, worldXsize, worldYsize);
    
#if 1
    image = [[NSImage alloc] initWithSize:aRect.size];
    imageRep = [[NSBitmapImageRep alloc]
                initWithBitmapDataPlanes:NULL
                pixelsWide:aRect.size.width
                pixelsHigh:aRect.size.height
                bitsPerSample:8
                samplesPerPixel:3
                hasAlpha:NO
                isPlanar:NO
                colorSpaceName:NSDeviceRGBColorSpace
                bytesPerRow:aRect.size.width*3
                bitsPerPixel:24];
    [image addRepresentation:imageRep];
#endif
}

- (id <GridData>)discrete2d
{
    return discrete2d;
}


// code to make a probe for an object at a specific point. This is
// good to make as a button client for Raster widgets
- (void)makeProbeAtX: (unsigned)x Y: (unsigned)y
{
    id obj;
    
    if (x >= 0
        && x < [discrete2d getSizeX]
        && y >= 0
        && y < [discrete2d getSizeY])
    {
        obj = [discrete2d getObjectAtX: x Y: y];
#ifndef SWARM_OSX
        if (obj)
            CREATE_PROBE_DISPLAY (obj);
        else
            GUI_BEEP ();
#endif
    }
    else
        raiseEvent (WarningMessage, 
                    "Object2dDisplay: invalid coordinates to make probe (%d,%d)\n",
                    x, y);
}

// Draw all objects in the array on an internal image.
// All that happens here is the display message
// is sent to each object - it is the object's responsibility to
// render itself. 

- (void)updateDisplay
{
    if (!image) [self createImage];
    
    if (image) {
        NSSize aSize = [image size];
        //printf("Object2dDisplay display: %f %f\n", aSize.width, aSize.height);
        NSRect aRect = NSMakeRect(0, 0, aSize.width, aSize.height);
        
        [image lockFocus];
        
        [[NSColor clearColor] set];
        NSRectFill(aRect);
        
        int x, y;
        for (x = 0; x < aSize.width; ++x)
            for (y = 0; y < aSize.height; ++y)
            {
                aRect = NSMakeRect(x, y, 1, 1);
                id obj = [discrete2d getObjectAtX: x Y: y];
                if (obj) {
                    if (displayInvocation) {
                        [displayInvocation setArgument: &aRect atIndex: 2];
                        [displayInvocation invokeWithTarget: obj];
                    } else
                        [obj drawRect: aRect];
                }
            }
        [image unlockFocus];
    }
}

@end
