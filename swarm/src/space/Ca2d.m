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

#import <space/Ca2d.h>
#import <defobj.h>

@implementation Ca2d

PHASE(Creating)

- setNumStates: (unsigned)n
{
  numStates = n;
  return self;
}

- initializeLattice
{
  raiseEvent (SubclassMustImplement,
              "initializeLattice must be implemented by Ca2d subclass");
  return nil;
}

- createEnd
{
  // allocate buffers.
  if (numStates == 0)
    raiseEvent (InvalidCombination, "CA not initialized correctly.\n");

  [super createEnd];
  
  // initialize ourselves.
  [self initializeLattice];
  return self;
}
PHASE(Setting)
PHASE(Using)

- stepRule
{
  raiseEvent (SubclassMustImplement, "Ca2d: no default step.\n");
  return nil;
}

@end
