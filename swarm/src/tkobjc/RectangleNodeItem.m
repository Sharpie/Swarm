// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <tkobjc/RectangleNodeItem.h>

@implementation RectangleNodeItem

- createItem
{
  //font and size independence means I have to make a fake label first...
  text = 
    strdup ([[globalTkInterp 
               eval: 
                 "%s create text %d %d -text \"%sxx\" -anchor c", 
               [canvas getWidgetName],
               x, y,
               string] result]);
  
  item =
    strdup ([[globalTkInterp
               eval: 
                 "set temp [%s bbox %s]; "
               "set h [expr ([lindex $temp 3] - [lindex $temp 1]) / 2]; "
               "%s create rectangle [lindex $temp 0] "
               "[expr [lindex $temp 1] - $h] [lindex $temp 2] "
               "[expr [lindex $temp 3] + $h] -fill white", 
               [canvas getWidgetName],
               text,
               [canvas getWidgetName]] result]);
  
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName],
                  text];

  free ((void *)text);

  text =
    strdup ([[globalTkInterp
               eval: 
                 "%s create text %d %d -text \"%s\" -font %s -anchor c", 
               [canvas getWidgetName],
               x, y, string, font] result]);
  
  [self createBindings];

  return self;
}

@end
