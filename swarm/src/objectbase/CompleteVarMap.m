// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CompleteVarMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // Warning
#import <defobj/defalloc.h> // getZone

#include <swarmconfig.h>
#ifdef HAVE_JDK
#import <defobj/JavaProxy.h>
#import "../defobj/java.h" // SD_JAVA_FIND_OBJECT_JAVA
#endif

#import "local.h"

@implementation CompleteVarMap

PHASE(Creating)
- createEnd
{
  Class aClass;
  id classList;  // added to ensure the vars are added from Object downwards
  id anIndex;    // as required by the ObjectSaver (for example)
	
  if (SAFEPROBES)
    if (!probedClass)
      {
        raiseEvent (WarningMessage,
                    "CompleteVarMap object was not properly initialized\n");
        return nil;
      }

  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
	
#ifdef HAVE_JDK
  if ([probedClass respondsTo: M(isJavaProxy)])
    { 
      jclass currentClass, nextClass;
      jclass classObject;

      classObject = SD_JAVA_FIND_OBJECT_JAVA (probedClass);

      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      [self addJavaFields: classObject];

      count = 0;
      for (currentClass = (*jniEnv)->GetSuperclass (jniEnv, classObject);
           currentClass;
           nextClass = (*jniEnv)->GetSuperclass (jniEnv, currentClass),
             (*jniEnv)->DeleteLocalRef (jniEnv, currentClass),
             currentClass = nextClass)
        [self addJavaFields: currentClass];
      return self;
    }
#endif

  classList = [List create: getZone (self)];
  if(!classList) 
    return nil;

  count = 0;

  aClass = probedClass;
  do {
    [classList addFirst: (id) aClass];
    aClass = aClass->super_class;
  } while (aClass);
  
  anIndex = [classList begin: getZone (self)];
  while ((aClass = (id) [anIndex next]))
    [self addObjcFields: aClass];
  [anIndex drop];
  [classList drop];
  return self;
}

PHASE(Using)
@end

