// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <collections/String.h>
#import <tkobjc/ButtonPanel.h>
#import <tkobjc/Button.h>
#import <tkobjc/global.h>
#include <misc.h> // strdup

@implementation ButtonPanel

- setButtonTarget: object
{
  targetName = strdup ([object getObjectName]);
  return self;
}

// this is atrocious - we should maintain a collection of the buttons.
- addButtonName: (const char *)n Command: (const char *)c
{
  Button * b;

  b = [Button createParent: self];
  [b setText: n];
  [b setCommand: c];
  // this command is unfortunate.
  [globalTkInterp eval: "%s configure -width 12", [b getWidgetName]];
  [b pack];
  // now save b away in a list. (unimplemented)

  return self;
}

- addButtonName: (const char *)n
     actionName: (const char *)action
{
  id string = [String create: [self getZone] setC: targetName];
  
  [string catC: " "];
  [string catC: action];
  return [self addButtonName: n Command: [string getC]];
}
@end
