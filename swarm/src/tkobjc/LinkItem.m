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

#import <tkobjc/global.h>
#import <tkobjc/Widget.h>
#import <tkobjc/LinkItem.h>
#import <tkobjc/NodeItem.h>
#import <defobj.h> // STRDUP

@implementation LinkItem

PHASE(Creating)

+ createBegin: aZone
{
  LinkItem *obj = [super createBegin: aZone];

  obj->directedFlag = YES;
  return obj;
}

- setDirectedFlag: (BOOL)theDirectedFlag
{
  directedFlag = theDirectedFlag;

  return self;
}

- setFrom: (id <NodeItem>)fromNode
{
  from = fromNode;

  return self;
}

- setTo: (id <NodeItem>)toNode
{
  to = toNode;

  return self;
}

- (void)createItem
{
  int fx,fy,tx,ty,mx,my;
  
  fx = [from getX];
  fy = [from getY];

  tx = [to getX];  
  ty = [to getY];

  mx = fx + (tx - fx) / 2;
  my = fy + (ty - fy) / 2;

  if (!directedFlag)
    {
      line1 = STRDUP (([[globalTkInterp 
                          eval: 
                            "%s create line %d %d %d %d", 
                          [canvas getWidgetName], fx, fy, tx, ty] 
                         result]));
      
      [globalTkInterp eval: "%s lower %s",
                      [canvas getWidgetName], line1];
      line2 = NULL;
    }
  else
    {
      line1 = STRDUP (([[globalTkInterp 
                          eval: 
                            "%s create line %d %d %d %d -arrow last", 
                          [canvas getWidgetName], fx, fy, mx, my] 
                         result]));
      
      line2 = STRDUP (([[globalTkInterp
                          eval: 
                            "%s create line %d %d %d %d", 
                          [canvas getWidgetName], mx, my, tx, ty] 
                         result]));
      
      [globalTkInterp eval: "%s lower %s; %s lower %s",
                      [canvas getWidgetName], line1,
                      [canvas getWidgetName], line2];
    }
}

- (void)createBindings
{
}

PHASE(Using)

- (void)update
{
  int fx, fy, tx, ty, mx, my;
  
  fx = [from getX];
  fy = [from getY];

  tx = [to getX];  
  ty = [to getY];

  mx = fx + (tx - fx) / 2;
  my = fy + (ty - fy) / 2;

  if (!directedFlag)
    [globalTkInterp eval: "%s coords %s %d %d %d %d",
      [canvas getWidgetName], line1, fx, fy, tx, ty];
  else
    {
      [globalTkInterp eval: "%s coords %s %d %d %d %d",
                      [canvas getWidgetName], line1, fx, fy, mx, my];
      
      [globalTkInterp eval: "%s coords %s %d %d %d %d",
                      [canvas getWidgetName], line2, mx, my, tx, ty];
    }
}

- (void)setColor: (const char *)aColor
{
  if (!directedFlag)
    [globalTkInterp eval: "%s itemconfigure %s -fill %s",
            [canvas getWidgetName], line1, aColor];
  else
    {
      [globalTkInterp eval: "%s itemconfigure %s -fill %s",
                      [canvas getWidgetName], line1, aColor];
      
      [globalTkInterp eval: "%s itemconfigure %s -fill %s",
                      [canvas getWidgetName], line2, aColor];
    }
}

- (void)drop
{
  [globalTkInterp eval: "%s delete %s %s",
                  [canvas getWidgetName], line1, line2];  

  if (line1)
    FREEBLOCK (line1);
  if (line2)
    FREEBLOCK (line2);
  [super drop];
}

@end

