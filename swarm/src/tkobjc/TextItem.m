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

#import <tkobjc/TextItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>
#import <defobj/defalloc.h> // getZone

@implementation TextItem

PHASE(Creating)

+ createBegin: aZone
{
  TextItem *obj = [super createBegin: aZone];
  
  obj->centerFlag = YES;
  
  return obj;
}

- setX: (int)the_x Y: (int)the_y
{
  x = the_x;
  y = the_y;

  return self;
}

- setText: (const char *)the_text
{
  text = the_text;

  return self;
}

- setFont: (const char *)the_font
{
  font = the_font;

  return self;
}

- setCenterFlag: (BOOL)theCenterFlag
{
  centerFlag = theCenterFlag;

  return self;
}
 
- createItem
{
  item = tkobjc_createText (getZone (self),
                            canvas, x, y, text, font, centerFlag);

  return self;
}

PHASE(Using)

@end
