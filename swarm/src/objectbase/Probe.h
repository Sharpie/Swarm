// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <objectbase/SwarmObject.h>

// Options for the format of the string returned when probing an unsigned
// char or a char (there is a choice between "%d %c", "%c" or "%d"...)

extern id <Symbol> DefaultString, CharString, IntString;

@interface Probe: SwarmObject
{
  Class probedClass;
  const char *probedType;
  int safety;
  id <Symbol> stringReturnType ;
  id objectToNotify;  // could be an object or a list
}

+ createBegin: aZone;
- createEnd;

- setObjectToNotify: anObject;
- getObjectToNotify;

- setProbedClass: (Class)aClass;
- createEnd;

- clone: aZone;

- (Class)getProbedClass;
- (const char *)getProbedType;

- setSafety;
- unsetSafety;

- setStringReturnType: returnType;
- setFloatFormat: (const char *)format;

- (void *)probeRaw: anObject;
- (void *)probeAsPointer: anObject;
- (int)   probeAsInt: anObject;
- (double)probeAsDouble: anObject;
- (const char *)probeAsString: anObject Buffer: (char *)buffer;
- (const char *)probeAsString: (id) anObject Buffer: (char *)buf 
            withFullPrecision: (int) precision;

@end
