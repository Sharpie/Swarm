// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections/String.h>
#import <awtobjc/ButtonPanel.h>
#import <awtobjc/Button.h>

#include <string.h>

@implementation ButtonPanel

- setButtonTarget: object
{
  targetName = strdup ([object getObjectName]);
  return self;
}

// this is atrocious - we should maintain a collection of the buttons.
- addButtonName: (const char *)n Command: (const char *)c
{
  Button *b = [Button createParent: self];
  [b setText: n];
  [b setCommand: c]; 

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
