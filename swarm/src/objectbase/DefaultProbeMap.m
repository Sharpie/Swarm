// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/DefaultProbeMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // Warning

#import "local.h"

@implementation DefaultProbeMap

- createEnd
{
  IvarList_t ivarList;
  int i;
  id a_probe ;

  if (SAFEPROBES)
    if (probedClass == 0)
      {
        raiseEvent (WarningMessage,
                    "DefaultProbeMap object was not properly initialized\n");
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
      numEntries = 0;
      classObject = SD_FINDJAVA (jniEnv, probedClass);
      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      
      [self addJavaFields: classObject];
      return self;
    }
#endif

  if (!(ivarList = probedClass->ivars))
    numEntries = 0;
  else 
    {
      numEntries = ivarList->ivar_count;
      
      for (i = 0; i < numEntries; i++)
        {
          const char *name;
          
          name = ivarList->ivar_list[i].ivar_name;
          
          a_probe = [VarProbe createBegin: [self getZone]];
          [a_probe setProbedClass: probedClass];
          [a_probe setProbedVariable: name];
          if (objectToNotify != nil) 
            [a_probe setObjectToNotify: objectToNotify];
          a_probe = [a_probe createEnd];
          
          [probes at: [String create: [self getZone] setC: name]
                  insert: a_probe];
        }
    }
  return self;
}

@end

