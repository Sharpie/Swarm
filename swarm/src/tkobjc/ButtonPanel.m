// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/ButtonPanel.h>
#import <tkobjc/global.h>

@implementation ButtonPanel
PHASE(Creating)

PHASE(Using)

- (void)setButtonTarget: object
{
  target = object;
}

// this is atrocious - we should maintain a collection of the buttons.
- (void)addButtonName: (const char *)name target: theTarget method: (SEL)sel
{
  id <Button> b;

  b = [Button createParent: self];
  [b setText: name];
  [b setButtonTarget: theTarget method: sel];
#if 0
  // this command is unfortunate.
  [globalTkInterp eval: "%s configure -width 12", [b getWidgetName]];
#endif
  [b pack];
  // now save b away in a list. (unimplemented)
}

- (void)addButtonName: (const char *)name method: (SEL)sel
{
  [self addButtonName: name target: target method: sel];
}
@end
