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
#import <tkobjc/ButtonPanel.h>
#import <tkobjc/global.h>

@implementation ButtonPanel
PHASE(Creating)

PHASE(Using)

- (void)setButtonTarget: object
{
  target = object;
}

// this is atrocious - we should maintain a collection of the buttons.
- (void)addButtonName: (const char *)name target: theTarget method: (SEL)sel
{
  id <Button> b;

  b = [Button createParent: self];
  [b setText: name];
  [b setButtonTarget: theTarget method: sel];
#if 0
  // this command is unfortunate.
  [globalTkInterp eval: "%s configure -width 12", [b getWidgetName]];
#endif
  [b pack];
  // now save b away in a list. (unimplemented)
}

- (void)addButtonName: (const char *)name method: (SEL)sel
{
  [self addButtonName: name target: target method: sel];
}
@end
