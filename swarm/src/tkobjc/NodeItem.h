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

#import <tkobjc/CompositeItem.h>
#import <gui.h>

@interface NodeItem: CompositeItem <NodeItem>
{
  int x, y;
  const char *item;
  const char *text;
  const char *string;
  const char *font;
}

- setX: (int)x Y: (int)y;
- setString: (const char *)string;
- setFont: (const char *)font;

- (int)getX;
- (int)getY;
- (void)resetString: (const char *)string;
- (void)setColor: (const char *)aColor;
- (void)setBorderColor: (const char *)aColor;
- (void)setBorderWidth: (int)aVal;
- (void)createBindings;
- (void)createText;
- (void)createPaddedText;
@end

