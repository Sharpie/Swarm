// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// new widget: collects a bunch of buttons into a frame, displays it.

#import <tkobjc/Frame.h>
#import <gui.h>

@interface ButtonPanel: Frame <ButtonPanel>
{
  id target;
}

- (void)setButtonTarget: target;
- (void)addButtonName: (const char *)n target: target method: (SEL)sel;
- (void)addButtonName: (const char *)n method: (SEL)sel;
@end
