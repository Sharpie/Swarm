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

#import <tkobjc/CanvasItem.h>
#import <tkobjc/Widget.h>
#import <tkobjc/global.h>
#import <defobj/defalloc.h>

@implementation CanvasItem

PHASE(Creating)

- (void)createBindings
{
  const char *temp = [self getObjectName];
  const char *canvasName = [canvas getWidgetName];

  [globalTkInterp eval: "%s bind %s <Button-3> {%s clicked}", 
                  canvasName, item, temp];
  [globalTkInterp eval: "%s bind %s <Button-1> {set curX %s; set curY %s}",
                  canvasName, item, "%x" , "%y"];

  [globalTkInterp eval: "%s bind %s <B1-Motion> {"
                  "%s initiateMoveX: [expr %s -$curX] Y: [expr %s -$curY];"
                  "set curX %s; set curY %s}",
                  canvasName,
                  item,
                  temp,
                  "%x", "%y", "%x", "%y"];
}

- createEnd
{
  [super createEnd];
  setMappedAlloc (self);
  return self;
}

PHASE(Using)

- (void)initiateMoveX: (long)deltaX Y: (long)deltaY
{
  if (moveSel && target)
    if ([target perform: moveSel with: (id)deltaX with: (id)deltaY])
      [globalTkInterp eval: "%s move %s %ld %ld; set curX %s; set curY %s",
                      [canvas getWidgetName],
                      item,
                      deltaX, deltaY,
                      "%x", "%y"];
  
  if (postMoveSel && target)
    [target perform: postMoveSel];
}

- (void)mapAllocations: (mapalloc_t)mapalloc
{
}

- (void)dropAllocations: (BOOL)componentAlloc
{
  if (componentAlloc && item)
    FREEBLOCK (item);
  [super dropAllocations: componentAlloc];
}


- (void)drop
{
  [globalTkInterp eval: "%s delete %s", [canvas getWidgetName], item];  
  [super drop];
}

@end

