// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <awtobjc/global.h>
#import <awtobjc/Widget.h>
#import <awtobjc/Circle.h>

@implementation Circle

- setX: (int)the_x Y: (int)the_y 
{
  x = the_x;
  y = the_y;
  return self;
}
 
- setRadius: (int)the_radius
{
  r = the_radius;
  return self;
}

- createItem
{
  abort ();
  return self;
}

- reportClick 
{
  printf("Reporting!!!\n");
  return self;
}

- (int)reportMoveX: (int)d_x Y: (int)d_y
{
  return 1;
}

@end

