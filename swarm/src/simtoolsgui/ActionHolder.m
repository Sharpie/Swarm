// Swarm library. Copyright (C) 1997-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/ActionHolder.h>

@implementation ActionHolder

PHASE(Creating)

- setActionTarget: tgt
{
  target = tgt;
  return self;
}

- getActionTarget
{
  return target;
}

- setSelector: (SEL)slctr
{
  selector = slctr;
  return self;
}

- setActionName: (const char *)nme
{
  name = nme;
  return self;
}

- setType: (id <Symbol>) tp
{
  type = tp;
  return self;
}

PHASE(Using)

- (SEL)getSelector
{
  return selector;
}

- (const char *)getActionName
{
  return name;
}

- (id <Symbol>) getType
{
  return type;
}

@end
