// Template application. Copyright (C) 1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

@interface ActionHolder : SwarmObject {
  id <Symbol> type;
  char * name;
  id target;
  SEL selector;
}

// Create methods
-createEnd;

// Use methods
-setActionTarget: (id) tgt;
-getActionTarget;
-setSelector: (SEL)  slctr;
-(SEL) getSelector;
-setActionName: (char *) nme;
-(char *) getActionName;
-setType: (id <Symbol>) tp;
-(id <Symbol>) getType;

@end
