// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/CanvasAbstractItem.h>
#import <gui.h>

@interface CompositeItem: CanvasAbstractItem <CompositeItem>
{
}

- (void)moveX: (long)deltaX Y: (long)deltaY;
- (void)initiateMoveX: (long)deltaX Y: (long)deltaY;
@end


