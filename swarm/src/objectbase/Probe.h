// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <objectbase/SwarmObject.h>

// Options for the format of the string returned when probing an unsigned
// char or a char (there is a choice between "%d %c", "%c" or "%d"...)

@interface Probe: SwarmObject
{
  Class probedClass;
  const char *probedType;
  int safety;
  unsigned isJavaProxy;
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
@end
