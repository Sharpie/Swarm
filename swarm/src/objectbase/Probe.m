// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/Probe.h>
#import <collections.h>
#import <defobj.h> // Warning
#import "local.h"

@implementation Probe
PHASE(Creating)

+  createBegin: aZone 
{
  Probe *obj = [super createBegin: aZone];

  obj->objectToNotify = nil;
  obj->language = LanguageObjc;
  return obj;
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

PHASE(Setting)
- setSafety
{
  safety = YES;
  return self;
}

PHASE(Using)

- setObjectToNotify: anObject
{
#if 0
  if (anObject != nil
      && (![anObject
             respondsTo:
               M(eventOccurredOn:via:withProbeType:on:ofType:withData:)])
      && (![anObject respondsTo: M(forEach:)]))
    raiseEvent (NotImplemented,
                "Object %0#p of class %s does not implement\n"
                "standard probe hook message.\n", 
                anObject,
                [[anObject class] name]);
#endif
  
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
  if ([aClass respondsTo: M(isJavaProxy)])
    language = LanguageJava;
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

- unsetSafety
{
  safety = NO;
  return self;
}

- clone: aZone
{
  [self subclassResponsibility: @selector(clone)];
  return self;
}

@end
