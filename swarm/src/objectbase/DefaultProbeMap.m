// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/DefaultProbeMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // Warning

#import "local.h"

#ifdef HAVE_JDK
#import "../defobj/directory.h" // java_ensure_selector
extern jmethodID  m_ClassGetDeclaredFields,
  m_FieldGetName;
#endif

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
      jarray fields;
      jsize fieldslength;
      unsigned i;
      
      numEntries = 0;
      classObject = JFINDJAVA(jniEnv, probedClass);
      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      
      if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv, classObject, 
						  m_ClassGetDeclaredFields)))
	abort(); 
      fieldslength = (*jniEnv)->GetArrayLength (jniEnv, fields);
      
      if (fieldslength)
	{
	  numEntries = fieldslength;
	  
	  for (i=0; i<numEntries; i++)
	    {
	      jobject field;
	      jstring name;
	      const char * buf;
	      jboolean isCopy;
	      
	      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, i);
	      
	      name = (*jniEnv)->CallObjectMethod (jniEnv, field,
						  m_FieldGetName);
	      
	      buf = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
	      
	      a_probe = [VarProbe createBegin: [self getZone]];
	      [a_probe setProbedClass: probedClass];
	      [a_probe setProbedVariable: buf];
	      
	      if (objectToNotify != nil) 
		[a_probe setObjectToNotify: objectToNotify];
	      a_probe = [a_probe createEnd];
	      
	      [probes at: [String create: [self getZone] setC: buf]
		      insert: a_probe];
	      
	      if (isCopy)
		(*jniEnv)->ReleaseStringUTFChars (jniEnv, name, buf);

	    }
	}

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

