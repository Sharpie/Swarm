// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/Circle.h>

@implementation Circle

-setX: (int) the_x Y: (int) the_y {
  x = the_x ;
  y = the_y ;
  return self ;
}
 
-setRadius: (int) the_radius {
  r = the_radius ;
  return self ;
}

-createItem {

  [globalTkInterp eval: 
    "%s create oval %d %d %d %d -fill white", 
    [canvas getWidgetName],x - r, y - r, x + r, y + r];
  
  item = strdup([globalTkInterp result]) ;

  return self;
}

-reportClick {
  printf("Reporting!!!\n") ;
  return self ;
}

-(int)reportMoveX: (int) d_x Y: (int) d_y {
  return 1 ;
}

@end

