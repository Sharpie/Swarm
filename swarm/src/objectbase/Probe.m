// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#include <stdio.h>

#import <objectbase/Probe.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation Probe
+  createBegin: aZone 
{
  Probe * tempObj;
  tempObj = [super createBegin: aZone];
  tempObj->objectToNotify = nil;
  return tempObj;
}

- createEnd
{
  [super createEnd];
  //return full description by default...
  stringReturnType = DefaultString;
  
  //by default we do not want to bother checking the class is valid
  //each time the probe is used...
  safety = 0;
  
  return self;
}

- setObjectToNotify: anObject
{
  if (anObject != nil
      && (![anObject
             respondsTo:
               M(eventOccurredOn:via:withProbeType:on:ofType:withData:)])
      && (![anObject respondsTo: M(forEach:)]))
    raiseEvent(NotImplemented, "Object %0#p of class %s does not implement"
	       "standard probe hook message.\n", 
	       anObject, [[anObject class] name]);
  
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          if ([anObject respondsTo: M(forEach:)])
            {
              // put all the objects on the ProbeMap's list 
              // at the time when we're created onto our list
              id index, tempObj;
              index = [anObject begin: scratchZone];
              while ( (tempObj = [index next]) != nil ) {
                if (([objectToNotify contains: tempObj]) == NO)
                  [objectToNotify addLast: tempObj];
              }
              [index drop];
            }
          else
            if (([objectToNotify contains: anObject]) == NO)
              [objectToNotify addLast: anObject];
        }
      else
        {  // objectToNotify is not a list
          id temp;
          temp = objectToNotify;
          objectToNotify = [List create: [self getZone]];
          [objectToNotify addLast: temp];
          if ([objectToNotify contains: anObject] == NO)
            [objectToNotify addLast: anObject];
        }
    }
  else
    objectToNotify = anObject;
  
  return self;
}
- getObjectToNotify
{
  return objectToNotify;
}

- setProbedClass: (Class)aClass
{
  if (SAFEPROBES)
    {
      if (probedClass != 0)
        {
          fprintf(stderr, "It is an error to reset the class\n");
          return nil;
        }
    }
  probedClass = aClass;
  return self;
}

- (Class)getProbedClass
{
  return probedClass;
}

- (const char *)getProbedType
{
  return probedType;
}

- setStringReturnType: returnType
{
  stringReturnType = returnType;
  return self;
}

- setFloatFormat: (const char *)format
{
  [self subclassResponsibility: @selector (setFloatFormat)];
  return self;
}

- setSafety
{
  safety = 1;
  return self;
}

- unsetSafety
{
  safety = 0;
  return self;
}

- clone: aZone
{
  [self subclassResponsibility: @selector(clone)];
  return self;
}

- (void *)probeRaw: anObject
{
  [self subclassResponsibility: @selector(probeRaw)];
  return self;
}

- (void *)probeAsPointer: anObject
{
  [self subclassResponsibility: @selector(probeAsPointer)];
  return self;
}

- (int)probeAsInt: anObject
{
  [self subclassResponsibility: @selector(probeAsInt)];
  return 0;
}

- (double)probeAsDouble: anObject
{
  [self subclassResponsibility: @selector(probeAsDouble)];
  return 0.0;
}

- (const char *)probeAsString: anObject Buffer: (char *)buf
{
  [self subclassResponsibility: @selector(probeAsString)];
  return NULL;
}

- (const char *)probeAsString: anObject Buffer: (char *)buf
            withFullPrecision: (int) precision
{
  [self subclassResponsibility: @selector(probeAsString)];
  return NULL;
}

@end
