// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/ButtonPanel.h>
#import <tkobjc/global.h>
#include <misc.h> // strdup

@implementation ButtonPanel

PHASE(Using)

- setButtonTarget: object
{
  target = object;
  return self;
}

// this is atrocious - we should maintain a collection of the buttons.
- addButtonName: (const char *)name target: theTarget method: (SEL)sel
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

  return self;
}

- addButtonName: (const char *)name method: (SEL)sel
{
  return [self addButtonName: name target: target method: sel];
}
@end
