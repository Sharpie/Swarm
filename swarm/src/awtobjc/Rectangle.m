// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <javaobjc/global.h>
#import <javaobjc/Widget.h>
#import <javaobjc/Rectangle.h>

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
  abort ();
  return self;
}

@end
