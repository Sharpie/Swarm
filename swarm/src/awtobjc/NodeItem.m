// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <javaobjc/global.h>
#import <javaobjc/Widget.h>
#import <javaobjc/NodeItem.h>

#import <javaobjc/JavaInput.h>

@implementation NodeItem

- setX: (int)the_x Y: (int)the_y
{
  x = the_x;
  y = the_y;
  return self;
}

- setColor: (const char *)aColor
{
  return self;
}

- setBorderColor: (const char *)aColor
{
  return self;
}

- setBorderWidth: (int)aVal
{
  return self;
}

- setString: (const char *)the_text
{
  string = the_text;
  return self;
}

- createBindings
{
  return self;
}

- (int)getX
{
  return x;
}

- (int)getY
{
  return y;
}

- moveX: (long)the_x Y: (long)the_y
{
  x += the_x;
  y += the_y;
  
  fprintf(stderr, "moveX!!!  x=%d  y=%d\n", (int)the_x, (int)the_y);
  abort (); // [canvas moveObj: self X: (int)x Y: (int)y];
  return self;
}

- movetoX: (long)the_x Y: (long)the_y
{
  fprintf(stderr, "move to X!!!  x=%d  y=%d\n", (int)x, (int)y);
  abort (); // [canvas moveObj: self X: (int)the_x Y: (int)the_y];

  x = the_x;
  y = the_y;
  
  return self;
}

@end

