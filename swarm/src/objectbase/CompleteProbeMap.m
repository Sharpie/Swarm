// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CompleteProbeMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // WarningMessage, raiseEvent
#include <swarmconfig.h>
#ifdef HAVE_JDK
#import "../defobj/java.h" // SD_JAVA_FIND_CLASS_JAVA, JNI
#endif

#import "local.h"

@implementation CompleteProbeMap
PHASE(Creating)
- createEnd
{
  IvarList_t ivarList;
  MethodList_t methodList;
  int i;
  id aProbe;
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
  
  probes = [Map createBegin: [self getZone]];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;


#ifdef HAVE_JDK
  if (isJavaProxy)
    classObject = SD_JAVA_FIND_CLASS_JAVA (probedClass);

  if (classObject)
    { 
      jclass currentClass, nextClass;

      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      
      numEntries = 0;
      
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

  classList = [List create: [self getZone]];
  if (!classList) 
    return nil;

  numEntries = 0;

  aClass = probedClass;
  do
    {
      [classList addFirst: (id) aClass];
      aClass = aClass->super_class;
    } 
  while(aClass);
  
  anIndex = [classList begin: [self getZone]];
  while ((aClass = (id) [anIndex next]))
    {
      if ((ivarList = aClass->ivars))
        {
          numEntries += ivarList->ivar_count;
          
          for (i = 0; i < ivarList->ivar_count; i++)
            {
              const char *name = ivarList->ivar_list[i].ivar_name;
              
              aProbe = [VarProbe createBegin: [self getZone]];
              [aProbe setProbedClass: aClass];
              [aProbe setProbedVariable: name];
              if (objectToNotify != nil) 
                [aProbe setObjectToNotify: objectToNotify];
              aProbe = [aProbe createEnd];
              
              [probes at: [String create: [self getZone] setC: name]
                      insert: aProbe];
            }
        }
      
      if ((methodList = aClass->methods))
        {
          numEntries += methodList->method_count;
          
          for (i = 0; i < methodList->method_count; i++)
            {
              aProbe = [MessageProbe createBegin: [self getZone]];
              [aProbe setProbedClass: probedClass];
              [aProbe setProbedSelector: methodList->method_list[i].method_name];
              if (objectToNotify != nil) 
                [aProbe setObjectToNotify: objectToNotify];
              aProbe = [aProbe createEnd];
              
              if (aProbe)
                [probes 
                  at: 
                    [String 
                      create: [self getZone] 
                      setC: 
                        sel_get_name (methodList->method_list[i].method_name)]
                  insert: aProbe];
            }
        }
    }
  [anIndex drop];
  [classList drop];

  return self;
}
PHASE(Using)
@end

