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
#import <tkobjc/CheckButton.h>

inline int
stringIsFalse (const char *s)
{
  return s[0] == '0' && s[1] == '\0';
}

@implementation CheckButton

PHASE(Creating)

- createEnd
{
  [super createEnd];
  
  // create the checkbutton
  [globalTkInterp eval: "checkbutton %s;", widgetName];
  [globalTkInterp eval: "%s configure -variable %s;",
                  widgetName,
		  variableName];
  return self;
}

PHASE(Using)

- (void)setBoolValue: (BOOL)v
{
  if (v)
    [globalTkInterp eval: "%s select;", widgetName];
  else
    [globalTkInterp eval: "%s deselect;", widgetName];
}

- (const char *)getValue
{
  return [globalTkInterp variableValue: variableName];
}

- (void)setValue: (const char *)v
{
  [self setBoolValue: stringIsFalse (v)];
}


// just ignore this entirely - does it mean anything?
- setWidth: (unsigned)w Height: (unsigned)h
{
  return self;
}

- (BOOL)getBoolValue
{
  return !stringIsFalse ([self getValue]);
}
@end
