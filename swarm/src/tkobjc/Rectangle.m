// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/Rectangle.h>

@implementation Rectangle

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

  [globalTkInterp 
    eval: 
      "%s create rectangle %d %d %d %d -fill white", 
    [canvas getWidgetName], tx, ty, lx, ly];
  
  item = strdup ([globalTkInterp result]);
  
  return self;
}

@end

