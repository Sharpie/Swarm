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
#import <tkobjc/NodeItem.h>

@implementation NodeItem

-setX: (int) the_x Y: (int) the_y {
  x = the_x ;
  y = the_y ;
  return self ;
}

-setColor: (char *) aColor {
  [globalTkInterp eval: "%s itemconfigure %s -fill %s",
    [canvas getWidgetName],item,aColor] ;  
  return self ;
}

-setBorderColor: (char *) aColor {
  [globalTkInterp eval: "%s itemconfigure %s -outline %s",
    [canvas getWidgetName],item,aColor] ;  
  return self ;
}

-setBorderWidth: (int) aVal {
  [globalTkInterp eval: "%s itemconfigure %s -width %d",
    [canvas getWidgetName],item,aVal] ;  
  return self ;
}

-setString: (char *) the_text {
  string = the_text ;
  return self ;
}

-createBindings {
  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
    [canvas getWidgetName], item, tclObjc_objectToName(self)] ;

  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
    [canvas getWidgetName], text, tclObjc_objectToName(self)] ;

  [globalTkInterp eval: "%s bind %s <Button-1> {
      set curX %s ; set curY %s
    }",[canvas getWidgetName], item, "%x" , "%y"];

  [globalTkInterp eval: "%s bind %s <Button-1> {
      set curX %s ; set curY %s
    }",[canvas getWidgetName], text, "%x" , "%y"];

  [globalTkInterp eval: "%s bind %s <B1-Motion> {
      %s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY] ;
      set curX %s ; set curY %s
    }",[canvas getWidgetName],item,tclObjc_objectToName(self),"%x","%y","%x","%y"];

  [globalTkInterp eval: "%s bind %s <B1-Motion> {
      %s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY] ;
      set curX %s ; set curY %s
    }",[canvas getWidgetName],text,tclObjc_objectToName(self),"%x","%y","%x","%y"];

  return self ;
}

-(int) getX {
  return x ;
}

-(int) getY {
  return y ;
}

-moveX: (long) the_x Y: (long) the_y {
  
  x += the_x ;
  y += the_y ;
  
  [globalTkInterp eval: "%s move %s %ld %ld ; %s move %s %ld %ld",
    [canvas getWidgetName], text, the_x, the_y,
    [canvas getWidgetName], item, the_x, the_y] ;

  return self ;
}

@end

