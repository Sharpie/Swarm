// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/CompleteProbeMap.h>
#import <collections.h>
#import <objc/objc-api.h>
#import <defobj.h> // WarningMessage, raiseEvent

#import "local.h"

#ifdef HAVE_JDK
#import "../defobj/directory.h" // java_ensure_selector
extern jclass c_Selector;
extern jmethodID  m_ClassGetDeclaredFields,
  m_ClassGetDeclaredMethods, m_MethodGetName,
  m_SelectorConstructor, m_FieldGetName;
#endif

@implementation CompleteProbeMap

- createEnd
{
  IvarList_t ivarList;
  MethodList_t methodList;
  int i;
  id a_probe;
  Class a_class;

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
    { 
      id inversionList;
      id index;
      jarray fields;
      jsize fieldslength;
      jarray methods;
      jsize methodslength;
      jclass currentClass;
      unsigned i;

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
	  
	  if (!(methods = (*jniEnv)->CallObjectMethod (jniEnv, currentClass, 
						       m_ClassGetDeclaredMethods)))
	    abort();
	  
	  methodslength = (*jniEnv)->GetArrayLength (jniEnv, methods);

	  numEntries += fieldslength;
	  
	  for (i=0; i<numEntries; i++)
	    {
	      jobject field;
	      jstring name;
	      const char * buf;
	      jboolean isCopy;
	      
	      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, i);
	      
	      name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
	      
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
	  
	  if (methodslength)
	    {
	      numEntries += methodslength;
	      
	      inversionList = [List create: [self getZone]];
	      
	      for (i=0; i<methodslength; i++)
		{
		  jobject method;
		  jstring name;
		  jobject selector;
		  SEL sel;
		  
		  method = (*jniEnv)->GetObjectArrayElement (jniEnv, methods, 
							     i);
		  name = (*jniEnv)->CallObjectMethod (jniEnv, method, 
						      m_MethodGetName);
		  selector = (*jniEnv)->NewObject (jniEnv, c_Selector, 
						   m_SelectorConstructor, 
						   currentClass,
						   name, 0);
		  sel = java_ensure_selector (jniEnv, selector);
		  
		  a_probe = [MessageProbe createBegin: [self getZone]];
		  [a_probe setProbedClass: probedClass];
		  [a_probe setProbedSelector: sel];
		  if (objectToNotify != nil) 
		    [a_probe setObjectToNotify: objectToNotify];
		  
		  a_probe = [a_probe createEnd];
	      
		  if(a_probe)
		    [inversionList addFirst: a_probe];
		  else
		    numEntries--;
		}
	      
	      index = [inversionList begin: [self getZone]];
	      while ((a_probe = [index next]))
		{
		  [probes at: 
			    [String 
			      create: [self getZone] 
			      setC: [a_probe getProbedMessage]] 
			  insert: 
			    a_probe];
		  [index remove];
		}	
	      [index drop];
	      [inversionList drop];
	    } 
	  currentClass = (*jniEnv)->GetSuperclass (jniEnv, currentClass);
	}
      return self;
    }
  
#endif

  classList = [List create: [self getZone]];
  if (!classList) 
    return nil;

  numEntries = 0;

  a_class = probedClass;
  do
    {
      [classList addFirst: (id) a_class];
      a_class = a_class->super_class;
    } 
  while(a_class);
  
  anIndex = [classList begin: [self getZone]];
  while ((a_class = (id) [anIndex next]))
    {
      if ((ivarList = a_class->ivars))
        {
          numEntries += ivarList->ivar_count;
          
          for (i = 0; i < ivarList->ivar_count; i++)
            {
              const char *name = ivarList->ivar_list[i].ivar_name;
              
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
      
      if ((methodList = a_class->methods))
        {
          numEntries += methodList->method_count;
          
          for (i = 0; i < methodList->method_count; i++)
            {
              a_probe = [MessageProbe createBegin: [self getZone]];
              [a_probe setProbedClass: probedClass];
              [a_probe setProbedSelector: methodList->method_list[i].method_name];
              if (objectToNotify != nil) 
                [a_probe setObjectToNotify: objectToNotify];
              a_probe = [a_probe createEnd];
              
              if (a_probe)
                [probes 
                  at: 
                    [String 
                      create: [self getZone] 
                      setC: 
                        sel_get_name (methodList->method_list[i].method_name)]
                  insert: a_probe];
            }
        }
    }
  [anIndex drop];
  [classList drop];

  return self;
}

@end

