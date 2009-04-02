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

/*
Name:         String.h
Description:  character string object
Library:      collections
*/

#import <Swarm/Create.h>
#import <Swarm/collections.h>

@interface String_c: CreateDrop_s <String>
{
@public
  char *string;
  int count;
  BOOL literalFlag;
}
/*** methods in String_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- createEnd;
+ create: aZone;
+ create: (id <Zone>)aZone setC: (const char *)cstring;
- setLiteralFlag: (BOOL)literalFlag;
- (void)setC: (const char *)cstring;
- copy: aZone;
- (const char *)getC;
- (void)catC: (const char *)cstring;
- (unsigned)getCount;
- (unsigned)count;
- (unsigned)length;
- (int)compare: aString;
- (BOOL)getLiteralFlag;
- lispIn: expr;
- hdf5In: hdf5Obj;
- (void)lispOutShallow: (id <OutputStream>)stream;
- (void)lispOutDeep: (id <OutputStream>)stream;
- (void)hdf5OutShallow: (id <HDF5>)hdf5Obj;
- (void)describe: outputCharStream;
- (void)mapAllocations: (mapalloc_t)mapalloc;
@end
