// Swarm library. Copyright � 1996-2000 Swarm Development Group.
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

// place holder for a map of probes. This will be replaced with the
// generic Map class. Given a class, build an array of probe objects that
// work on that class (one per variable).  This one only contains IVars.

#import <objectbase/ProbeMap.h>

@interface DefaultProbeMap: ProbeMap <DefaultProbeMap>
{
}

-createEnd;
@end
