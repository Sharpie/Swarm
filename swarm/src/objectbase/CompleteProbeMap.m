// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CompleteProbeMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // WarningMessage, raiseEvent
#import <defobj/defalloc.h> // getZone
#include <swarmconfig.h>
#ifdef HAVE_JDK
#import "../defobj/java.h" // SD_JAVA_FIND_CLASS_JAVA, JNI
#endif

#import "local.h"

@implementation CompleteProbeMap
PHASE(Creating)
- createEnd
{
  Class aClass;

  id classList;  //added to ensure the vars are added from Object downwards
  id anIndex;    //as required by the ObjectSaver (for example).

  if (SAFEPROBES)
    if (probedClass == 0)
      {
        raiseEvent (WarningMessage,
                    "CompleteProbeMap object was not properly initialized\n");
        return nil;
      }
  
  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
#ifdef HAVE_JDK
  if ([probedClass respondsTo: M(isJavaProxy)])
    {
      jclass currentClass, nextClass;
      classObject = SD_JAVA_FIND_CLASS_JAVA (probedClass);
      
      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found.\n");

      [self addJavaFields: classObject];
      [self addJavaMethods: classObject];
      
      for (currentClass = (*jniEnv)->GetSuperclass (jniEnv, classObject);
           currentClass;
           nextClass = (*jniEnv)->GetSuperclass (jniEnv, currentClass),
             (*jniEnv)->DeleteLocalRef (jniEnv, currentClass),
             currentClass = nextClass)
        {
          [self addJavaFields: currentClass];
          [self addJavaMethods: currentClass];
        }
      return self;
    }
#endif
  classList = [List create: getZone (self)];
  if (!classList) 
    return nil;

  aClass = probedClass;
  do
    {
      [classList addFirst: (id) aClass];
      aClass = aClass->super_class;
    } 
  while (aClass);
  
  anIndex = [classList begin: getZone (self)];
  while ((aClass = (id) [anIndex next]))
    {
      [self addObjcFields: aClass];
      [self addObjcMethods: aClass];
    }
  [anIndex drop];
  [classList drop];

  return self;
}
PHASE(Using)
@end

