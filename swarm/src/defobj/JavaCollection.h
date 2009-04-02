// Swarm library. Copyright © 2000 Swarm Development Group.
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

#import <Swarm/JavaProxy.h>
#import <Swarm/swarmconfig.h>

@interface JavaCollection: JavaProxy <Serialization>
{
}
#ifdef HAVE_JDK
- (BOOL)isJavaCollection;
- (unsigned)getCount;
- begin: aZone;
- beginPermuted: aZone;
- getFirst;
- (void)addLast: obj;
- (void)forEach: (SEL)sel :arg1;
- (void)describeForEach: stream;
- lispIn: expr;
- (void)lispOutDeep: stream;
- (void)lispOutShallow: stream;
- hdf5In: hdf5Obj;
- (void)hdf5OutDeep: hdf5Obj;
- (void)hdf5OutShallow: hdf5Obj;
#endif
@end
