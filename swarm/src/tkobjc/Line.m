// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/Line.h>

#include <misc.h>

@implementation Line

PHASE(Creating)

- setTX: (int)the_tx TY: (int)the_ty LX: (int)the_lx LY: (int)the_ly
{
  tx = the_tx;
  ty = the_ty;
  lx = the_lx;
  ly = the_ly;

  return self;
}
 
- createItem
{
  [globalTkInterp eval: 
    "%s create line %d %d %d %d", 
    [canvas getWidgetName], tx, ty, lx, ly];
  
  item = strdup ([globalTkInterp result]);

  return self;
}

PHASE(Using)

@end

