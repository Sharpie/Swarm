// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/OvalNodeItem.h>
#import <defobj.h> // STRDUP, FREEBLOCK

@implementation OvalNodeItem

PHASE(Creating)

- createItem
{
  [self createPaddedText];
   
  item =
    STRDUP (([[globalTkInterp
                eval: 
                  "set temp [%s bbox %s]; "
                "set h [expr ([lindex $temp 3] - [lindex $temp 1]) / 2];"
                "%s create oval [lindex $temp 0] [expr [lindex $temp 1] - $h] "
                "[lindex $temp 2] [expr [lindex $temp 3] + $h] -fill white", 
                [canvas getWidgetName],
                text,
               [canvas getWidgetName]] result]));
  
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName], text];

  FREEBLOCK (text);

  [self createText];
  [self createBindings];
  
  return self;
}

PHASE(Using)

@end
