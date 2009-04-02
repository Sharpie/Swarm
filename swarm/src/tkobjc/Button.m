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
#import <tkobjc/Button.h>

#import <defobj/swarm-objc-api.h>
#include <misc.h> // strcpy, stpcpy

@implementation Button

PHASE(Creating)

- createEnd
{
  [super createEnd];

  [globalTkInterp eval: "button %s", widgetName];
  
  return self;
}

PHASE(Using)

- (void)setText: (const char *)text
{
  [globalTkInterp eval: "%s configure -text {%s}", widgetName, text];
}

- (void)setButtonTarget: target method: (SEL)sel
{
  char bcmd[1024], *p;
  
  p = stpcpy (bcmd, [target getObjectName]);
  p = stpcpy (p, " ");
  strcpy (p, sel_get_name (sel));
  [globalTkInterp eval: "%s configure -command {%s}", widgetName, bcmd];
}

@end
