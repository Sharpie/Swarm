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

#import <simtoolsgui/SingleProbeDisplay.h>

@implementation SingleProbeDisplay

PHASE(Creating)

- setProbedObject: (id)anObject
{
  probedObject = anObject;

  return self;
}

- createEnd
{
  [super createEnd];
  [topLevel setWindowTitle: [self getId: NULL]];

  return self;
}

PHASE(Using)

- (void)setRemoveRef: (BOOL)theRemoveRef
{
  removeRef = theRemoveRef;
}

- (void)setObjectRef: (ref_t)theObjectRef
{
  objectRef = theObjectRef;
}

- getProbedObject
{
  return probedObject;
}

- (const char *)package: (const char *)windowName
{
  return [probedObject getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  return [probedObject getDisplayName];
}

@end
