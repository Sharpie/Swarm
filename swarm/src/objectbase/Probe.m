// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Probe.h>
#import <collections.h>
#import <defobj.h> // Warning
#import "local.h"

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
    raiseEvent (NotImplemented,
                "Object %0#p of class %s does not implement"
                "standard probe hook message.\n", 
                anObject,
                [[anObject class] name]);
  
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
    if (probedClass != 0)
      {
        raiseEvent (WarningMessage, "It is an error to reset the class\n");
        return nil;
      }
#ifdef HAVE_JDK
  // if class passed to setProbedClass is 
  isJavaProxy = [aClass respondsTo: M(isJavaProxy)];     
#endif

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

@end
