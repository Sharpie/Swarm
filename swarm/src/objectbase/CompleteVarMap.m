// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CompleteVarMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // Warning

#import "local.h"

#ifdef HAVE_JDK
#import "../defobj/directory.h" // java_ensure_selector
extern jmethodID  m_ClassGetDeclaredFields,
  m_FieldGetName;
#endif

@implementation CompleteVarMap

- createEnd
{
  IvarList_t ivarList;
  int i;
  id a_probe;
  Class a_class;

  id classList;  //added to ensure the vars are added from Object downwards
  id anIndex;    //as required by the ObjectSaver (for example).
	
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
      jarray fields;
      jsize fieldslength;
      unsigned i;
      jclass currentClass;

      classObject = JFINDJAVA(jniEnv, probedClass);

      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      currentClass = classObject;

      numEntries = 0;
      while (currentClass)
	{
	  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv, currentClass, 
						  m_ClassGetDeclaredFields)))
	    abort(); 
	  fieldslength = (*jniEnv)->GetArrayLength (jniEnv, fields);
      
	  if (fieldslength)
	    {
	      numEntries += fieldslength;
	  
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
	  currentClass = (*jniEnv)->GetSuperclass (jniEnv, currentClass);
	}

      return self;
    }
#endif

  classList = [List create: [self getZone]];
  if(!classList) 
    return nil;

  numEntries = 0;

  a_class = probedClass;
  do{
    [classList addFirst: (id) a_class];
    a_class = a_class->super_class;
  }while(a_class);

  anIndex = [classList begin: [self getZone]];
  while ((a_class = (id)[anIndex next]))
    {
      if ((ivarList = a_class->ivars))
        {
          numEntries += ivarList->ivar_count;
          
          for (i = 0; i < ivarList->ivar_count; i++)
            {
              char *name;
              
              name = (char *)ivarList->ivar_list[i].ivar_name;
              
              a_probe = [VarProbe createBegin: [self getZone]];
              [a_probe setProbedClass: a_class];
              [a_probe setProbedVariable: name];
              if (objectToNotify != nil) 
                [a_probe setObjectToNotify: objectToNotify];
              a_probe = [a_probe createEnd];
              
              [probes at: [String create: [self getZone] setC: name]
                      insert: a_probe];
            }
        }
    }
  [anIndex drop];
  [classList drop];
  return self;
}

@end

