// Swarm library. Copyright (C) 1996, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <awtobjc/global.h>
#import <awtobjc/Widget.h>
#import <awtobjc/Canvas.h>
#import <awtobjc/LinkItem.h>

@implementation LinkItem

- setFrom: the_from
{
  from = the_from;
  return self;
}

- setTo: the_to
{
  to = the_to;
  return self;
}

- update
{
  int fx,fy,tx,ty; // mx,my;
 
  fx = [from getX];
  fy = [from getY];

  tx = [to getX];  
  ty = [to getY];

  [canvas drawLineFX: fx FY: fy TX: tx TY: ty];

  return self;
}

- createItem
{
  int fx,fy,tx,ty; // ,mx,my;
 
  fx = [from getX];
  fy = [from getY];

  tx = [to getX];  
  ty = [to getY];

  [canvas drawLineFX: fx FY: fy TX: tx TY: ty];

  return self;
}

- createBindings
{
  return self;
}

- setColor: (const char *)aColor
{
  [canvas setColor: aColor];
 
  return self;
}

- (void)drop
{
  [super drop];
}

@end

