// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h> // VarProbe
#import <objectbase/Probe.h>

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
  void *fieldObject;
  void *fieldType;
  void *classObject;
#endif
}

- setProbedVariable: (const char *)aVariable;
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
- (const char *)probeAsString: (id) anObject Buffer: (char *)buf 
            withFullPrecision: (int) precision;

- (unsigned)getRank;
- (unsigned *)getDims;
- (const char *)getBaseType;

- iterateAsDouble: anObject using: (void (*) (unsigned rank, unsigned *vec, double val))func;
- iterateAsInteger: anObject using: (void (*) (unsigned rank, unsigned *vec, int val))func;

- setData: anObject To: (void *)newValue;	      // pass by reference.
- (BOOL)setData: anObject ToString: (const char *)s;  // gives us the string.
- (void)drop;

@end



