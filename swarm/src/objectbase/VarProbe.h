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

#import <Swarm/objectbase.h> // VarProbe
#import <Swarm/Probe.h>
#import <Swarm/COM.h> // COMmethod

#import <Swarm/swarmconfig.h>

@interface VarProbe: Probe <VarProbe>
{
  const char *probedVariable;
  int dataOffset;
  BOOL interactiveFlag;
  const char *floatFormat; // actual sprintf-type format

  unsigned rank;
  unsigned *dims;
  const char *baseType;

#ifdef HAVE_JDK
  JOBJECT *javaInfo;
#endif
  COMmethod getterMethod;
  COMmethod setterMethod;
}

- setProbedVariable: (const char *)aVariable;
- setProbedCOMgetter: (COMmethod)getter setter: (COMmethod)setter;
- createEnd;

- setNonInteractive;
- setStringReturnType: returnType;
- setFloatFormat: (const char *)format;

- (const char *)getProbedVariable;
- (int)getDataOffset;
- (BOOL)getInteractiveFlag;


- setStringReturnType: returnType;
- setFloatFormat: (const char *)format;

- (void *)probeRaw: anObject;
- probeObject: anObject;
- (void *)probeAsPointer: anObject;
- (int)probeAsInt: anObject;
- (double)probeAsDouble: anObject;
- (const char *)probeAsString: anObject Buffer: (char *)buffer;
- (id <String>)probeAsString: anObject;
- (const char *)probeAsString: nObject Buffer: (char *)buf 
            withFullPrecision: (BOOL)precision;

- (unsigned)getRank;
- (unsigned *)getDims;
- (const char *)getBaseType;

- iterateAsDouble: anObject using: (void (*) (unsigned rank, unsigned *vec, double val))func;
- iterateAsInteger: anObject using: (void (*) (unsigned rank, unsigned *vec, int val))func;

- (void)setData: anObject To: (void *)newValue;	      // pass by reference.
- (BOOL)setData: anObject ToString: (const char *)s;  // gives us the string.
- (void)setData: anObject ToDouble: (double)newValue;

- (void)drop;
- (void)describe: stream;
@end

