// Swarm library. Copyright (C) 1997-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface ActionHolder : SwarmObject
{
  id <Symbol> type;
  const char *name;
  id target;
  SEL selector;
}

// Creating methods
- setActionTarget: tgt;
- setSelector: (SEL)slctr;
- setActionName: (const char *)nme;
- setType: (id <Symbol>) tp;

// Use methods
- getActionTarget;
- (SEL)getSelector;
- (const char *)getActionName;
- (id <Symbol>)getType;

@end
