// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CompleteVarMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // Warning

#import "local.h"
#ifdef HAVE_JDK
#import <defobj/directory.h> // SD_FINDJAVA, jni types
#endif

@implementation CompleteVarMap

- createEnd
{
  IvarList_t ivarList;
  id aProbe;
  Class aClass;

  id classList;  // added to ensure the vars are added from Object downwards
  id anIndex;    // as required by the ObjectSaver (for example)
	
  if (SAFEPROBES)
    if (probedClass == 0)
      {
        raiseEvent (WarningMessage,
                    "CompleteVarMap object was not properly initialized\n");
        return nil;
      }

  probes = [Map createBegin: [self getZone]];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
	
  if (probes == nil)
    return nil;

#ifdef HAVE_JDK
  if (isJavaProxy)
    { 
      jclass currentClass;

      classObject = SD_FINDJAVA (jniEnv, probedClass);

      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      currentClass = classObject;

      numEntries = 0;
      for (currentClass = classObject;
           currentClass;
           currentClass = (*jniEnv)->GetSuperclass (jniEnv, currentClass))
        [self addJavaFields: currentClass];

      return self;
    }
#endif

  classList = [List create: [self getZone]];
  if(!classList) 
    return nil;

  numEntries = 0;

  aClass = probedClass;
  do {
    [classList addFirst: (id) aClass];
    aClass = aClass->super_class;
  } while(aClass);
  
  anIndex = [classList begin: [self getZone]];
  while ((aClass = (id) [anIndex next]))
    {
      if ((ivarList = aClass->ivars))
        {
          unsigned i;
          numEntries += ivarList->ivar_count;
          
          for (i = 0; i < ivarList->ivar_count; i++)
            {
              const char *name;
              
              name = ivarList->ivar_list[i].ivar_name;
              
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
    }
  [anIndex drop];
  [classList drop];
  return self;
}

@end

