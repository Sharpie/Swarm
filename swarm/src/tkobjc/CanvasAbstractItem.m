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

#import <tkobjc/CanvasAbstractItem.h>

@implementation CanvasAbstractItem

PHASE(Creating)

- setCanvas: (id <Canvas>)theCanvas
{
  canvas = theCanvas;
  return self;
}

- (void)createItem
{
  [self subclassResponsibility: @selector (createItem)];
}

- (void)createBindings
{
  [self subclassResponsibility: @selector (createBindings)];
}

- createEnd 
{
  [self createItem];
  [self createBindings];
  return [super createEnd];
}

PHASE(Using)

- (void)setTargetId: the_target
{
  target = the_target;
}    

- (void)setClickSel: (SEL)the_sel
{
  clickSel = the_sel;
}

- (void)setMoveSel: (SEL)the_sel
{
  moveSel = the_sel;
}

- (void)setPostMoveSel: (SEL)the_sel
{
  postMoveSel = the_sel;
}

- (void)clicked
{
  if (clickSel && target)
    [target perform: clickSel];
}

- (void)initiateMoveX: (long)delta_x Y: (long)delta_y
{
  [self subclassResponsibility: @selector(initiateMoveX:Y:)];
}

- (id <Canvas>)getCanvas
{
  return canvas;
}

@end

