// Template application. Copyright (C) 1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ActionHolder.h>
@implementation ActionHolder

// Create methods
-createEnd {
  [super createEnd];
  return self;
}

// Use methods
-setActionTarget: (id) tgt {
  target = tgt;
  return self;
}

-getActionTarget {
  return target;
}

-setSelector: (SEL)  slctr {
  selector = slctr;
  return self;
}

-(SEL) getSelector {
  return selector;
}

-setActionName: (char *) nme {
  name = nme;
  return self;
}

-(char *) getActionName {
  return name;
}

-setType: (id <Symbol>) tp {
  type = tp;
  return self;
}


-(id <Symbol>) getType {
  return type;
}


@end
