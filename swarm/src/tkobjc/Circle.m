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

#import <tkobjc/Circle.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>
#import <defobj.h> // STRDUP

@implementation Circle

PHASE(Creating)

- setX: (int)the_x Y: (int)the_y
{
  x = the_x;
  y = the_y;

  return self;
}
 
- setRadius: (unsigned)the_radius
{
  r = the_radius;

  return self;
}

- createItem
{
  [globalTkInterp eval: 
    "%s create oval %d %d %d %d -fill white", 
    [canvas getWidgetName], x - r, y - r, x + r, y + r];
  
  item = STRDUP ([globalTkInterp result]);

  return self;
}

PHASE(Using)

@end

