/* All Rights reserved */

#include <AppKit/AppKit.h>
#include "Diffuse2dView.h"

@implementation Diffuse2dView


- (void) fax: (id)sender
{
  /* insert your code here */
}


- (void) print: (id)sender
{
  /* insert your code here */
}

- (void)drawRect:(NSRect)aRect
{
    id color = [NSColor redColor];

    [self lockFocus];

    [color set];
    PSrectfill(0, 0, 20, 50);

    [self unlockFocus];
    [[self window] flushWindow];

/*
  [image compositeToPoint:aRect.origin 
         fromRect:aRect 
         operation:NSCompositeCopy];
*/
}

@end
