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

// Create methods
- createEnd;

// Use methods
- setActionTarget: tgt;
- getActionTarget;
- setSelector: (SEL)slctr;
- (SEL)getSelector;
- setActionName: (const char *)nme;
- (const char *)getActionName;
- setType: (id <Symbol>) tp;
- (id <Symbol>) getType;

@end
