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

// TopLevels and Frames are about the same, so we put these in one class.

#import <tkobjc/Frame.h>
#import <tkobjc/global.h>
#import <tkobjc/common.h>

@implementation Frame

PHASE(Creating)

- setBorderWidth: (unsigned)theBorderWidth
{
  borderWidth = borderWidth;
  return self;
}

- setReliefFlag: (BOOL)theReliefFlag
{
  reliefFlag = theReliefFlag;
  return self;
}

// make a new top level frame. Can't use Widget default createEnd, because
// this is where the toplevel is actually built.
- createEnd
{
  if (parent == nil)
    {
      [self setWidgetNameFromParentName: "."];
      [globalTkInterp eval: "toplevel %s; wm minsize %s 1 1",
                      widgetName, widgetName];
      [self registerAndLoad];
    }
  else
    {
      [super createEnd];
      tkobjc_makeFrame (self);
    }

  if (reliefFlag)
    [globalTkInterp eval: "%s configure -relief ridge -borderwidth 3",
                    widgetName];

  if (borderWidth > 0)
    [globalTkInterp eval: "%s configure -bd %d",
                    widgetName, borderWidth];

  return self;
}

PHASE(Using)

- (void)withdraw
{
  [globalTkInterp eval: "wm withdraw %s", [self getWidgetName]];
}

- (void)deiconify
{
  [globalTkInterp eval: "wm deiconify %s", [self getWidgetName]];
}

@end

