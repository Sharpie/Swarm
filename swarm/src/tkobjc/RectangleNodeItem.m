// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <tkobjc/RectangleNodeItem.h>
#import <tkobjc/Widget.h>
#include <tkobjc/global.h>
#include <defobj.h> // STRDUP

@implementation RectangleNodeItem

PHASE(Creating)

- createItem
{
  [self createPaddedText];

  item =
    STRDUP (([[globalTkInterp
                eval: 
                  "set temp [%s bbox %s]; "
                "set h [expr ([lindex $temp 3] - [lindex $temp 1]) / 2]; "
                "%s create rectangle [lindex $temp 0] "
                "[expr [lindex $temp 1] - $h] [lindex $temp 2] "
                "[expr [lindex $temp 3] + $h] -fill white", 
                [canvas getWidgetName],
                text,
                [canvas getWidgetName]] result]));
  
  [globalTkInterp eval: "%s delete %s",
                  [canvas getWidgetName],
                  text];

  FREEBLOCK (text);

  [self createText];
  [self createBindings];

  return self;
}

PHASE(Using)

@end
