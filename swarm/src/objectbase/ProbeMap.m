// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/ProbeMap.h>
#import <objc/objc-api.h>
#import <defobj.h> // Warning, STRDUP
#import <defobj/defalloc.h> // getZone

#import "local.h"

#include <swarmconfig.h> // HAVE_JDK
#import <defobj/directory.h> // SD_SUPERCLASS
#ifdef HAVE_JDK
#import "../defobj/java.h" // SD_JAVA_ENSUREOBJCMETHOD, SD_JAVA_FIND_OBJECT_JAVA, java_field_usable_p
#import "../defobj/javavars.h" // m_*, c_*
#endif

@implementation ProbeMap
PHASE(Creating)
+ createBegin: aZone
{
  ProbeMap *tempObj;

  tempObj = [super createBegin: aZone];
  tempObj->objectToNotify = nil;
  return tempObj;
}

- setObjectToNotify: anObject
{
  id temp_otn;

  if (anObject != nil
      && ([anObject 
            respondsTo:
              M(eventOccurredOn:via:withProbeType:on:ofType:withData:)] == NO)
      && ![anObject respondsTo: M(forEach:)])
    raiseEvent (NotImplemented, "Object %0#p of class %s does not implement "
                "standard probe hook message.\n", 
                anObject, [[anObject class] name]);
  
  
  // this is pretty ugly, if you set more than one thing to 
  // notify, you'll be invoking this code more than once with
  // possibly no effect.
  
  // inherit the probeLibrary's otn, which should NOT be a list
  if (objectToNotify != nil) {
    if ((temp_otn = [probeLibrary getObjectToNotify]) != nil)
      {
        if ([objectToNotify respondsTo: M(forEach:)])
          {
            if ([temp_otn respondsTo: M(forEach:)])
              {
                // both exist and both are lists
                // add contents of temp_otn to otn
                id index, tempObj;
                index = [temp_otn begin: scratchZone];
                while ( (tempObj = [index next]) != nil)
                  {
                    if (([objectToNotify contains: tempObj]) == NO)
                      [objectToNotify addLast: tempObj];
                  }
                [index drop];
              }
            else
              {
                // both exist, otn is list temp_otn not list
                // add temp_otn to otn
                if (([objectToNotify contains: temp_otn]) == NO)
                  [objectToNotify addLast: temp_otn];
              }
          }
        else if ([temp_otn respondsTo: M(forEach:)])
          {
            // both exist, otn is not list temp_otn is list
            // add otn to front of temp_otn and swap
            id tempObj;
            tempObj = objectToNotify;
            objectToNotify = temp_otn;
            if ([objectToNotify contains: tempObj] == NO)
              [objectToNotify addFirst: tempObj];
          }
      }
    //else clause => otn exists, temp_otn does not, so do nothing
  }
  else if ((temp_otn = [probeLibrary getObjectToNotify]) != nil)
    objectToNotify = temp_otn;
  //else clause => neither exist, so do nothing
  
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
              while ( (tempObj = [index next]) != nil )
                {
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
          objectToNotify = [List create: getZone (self)];
          [objectToNotify addLast: temp];
          if (([objectToNotify contains: anObject]) == NO) 
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

- _copyCreateEnd_
{
  if (SAFEPROBES)
    if (probedClass == 0)
      {
        raiseEvent (WarningMessage,
                    "ProbeMap object was not properly initialized\n");
        return nil;
      }
  numEntries = 0;
  
  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;
  
  return self;
}

#ifdef HAVE_JDK

- (void)addJavaFields: (jclass)javaClass
{
  jarray fields;
  jsize count;

  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv,
                                              javaClass,
                                              m_ClassGetDeclaredFields)))
    abort();

  count = (*jniEnv)->GetArrayLength (jniEnv, fields);

  while (count > 0)
    {
      jobject field;
      
      count--;
      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, count);
      if (java_field_usable_p (field))
	{
	  jstring name;
	  const char *buf;
	  jboolean isCopy;
	  id aProbe;

          name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
	  buf = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
	  aProbe = [VarProbe createBegin: getZone (self)];
	  [aProbe setProbedClass: probedClass];
	  [aProbe setProbedVariable: buf];
	  
	  if (objectToNotify != nil) 
	    [aProbe setObjectToNotify: objectToNotify];
	  aProbe = [aProbe createEnd];
	  
	  [probes at: [String create: getZone (self) setC: buf]
		  insert: aProbe];
	  
	  if (isCopy)
	    (*jniEnv)->ReleaseStringUTFChars (jniEnv, name, buf);
	  (*jniEnv)->DeleteLocalRef (jniEnv, name);
          numEntries++;
	}
      (*jniEnv)->DeleteLocalRef (jniEnv, field);
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, fields);
}

- (void)addJavaMethods: (jclass)javaClass
{
  jarray methods;
  jsize count;

  if (!(methods = (*jniEnv)->CallObjectMethod (jniEnv,
                                               javaClass,
                                               m_ClassGetDeclaredMethods)))
    abort();
  
  count = (*jniEnv)->GetArrayLength (jniEnv, methods);
  
  if (count)
    {
      while (count > 0)
        {
	  jobject method;
          
          count--;
            
	  method = (*jniEnv)->GetObjectArrayElement (jniEnv, methods, count);
	  if (java_method_usable_p (method))
	    {
              SEL sel;
              jobject selector;
	      jstring name = (*jniEnv)->CallObjectMethod (jniEnv,
							  method, 
							  m_MethodGetName);
	      id aProbe;

              selector = (*jniEnv)->NewObject (jniEnv,
                                               c_Selector, 
                                               m_SelectorConstructor, 
                                               javaClass,
                                               name,
                                               JNI_FALSE);
              (*jniEnv)->DeleteLocalRef (jniEnv, name);
              // selector construction can fail on polymorphic methods
              if (selector)
                {
                  sel = SD_JAVA_ENSUREOBJCMETHOD (selector);
                  (*jniEnv)->DeleteLocalRef (jniEnv, selector);
                  
                  aProbe = [MessageProbe createBegin: getZone (self)];
                  [aProbe setProbedClass: probedClass];
                  [aProbe setProbedSelector: sel];
                  if (objectToNotify != nil) 
                    [aProbe setObjectToNotify: objectToNotify];
                  
                  aProbe = [aProbe createEnd];
              
                  if (!aProbe)
                    abort ();
                  
                  [probes at: [String create: getZone (self)
                                      setC: [aProbe getProbedMessage]]
                          insert: aProbe];
                  numEntries++;
                }
            }
	  (*jniEnv)->DeleteLocalRef (jniEnv, method);
        }
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, methods);
}
#endif
  
- createEnd
{
  IvarList_t ivarList;
  MethodList_t methodList;
  id inversionList, index;

  //The compiler seems to put the methods in the 
  //opposite order than the one in which they were
  //declared, so we need to manually invert them.
  
  int i;
  id aProbe;
 
  if (SAFEPROBES)
    if (probedClass == 0)
      {
        raiseEvent (WarningMessage,
                    "ProbeMap object was not properly initialized\n");
        return nil;
      }

  if (objectToNotify == nil)
    [self setObjectToNotify: 
            [probeLibrary getObjectToNotify]];
  
  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];  

  if (probes == nil)
    return nil;

#ifdef HAVE_JDK
  if (isJavaProxy)
    { 
      classObject = SD_JAVA_FINDJAVACLASS (probedClass);
      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      numEntries = 0;

      [self addJavaFields: classObject];
      [self addJavaMethods: classObject];
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
          
          aProbe = [VarProbe createBegin: getZone (self)];
          [aProbe setProbedClass: probedClass];
          [aProbe setProbedVariable: name];
          if (objectToNotify != nil) 
            [aProbe setObjectToNotify: objectToNotify];
          aProbe = [aProbe createEnd];
          
          [probes at: [String create: getZone (self) setC: name]
                  insert: aProbe];
        }
    }
  
  if ((methodList = probedClass->methods))
    {
      numEntries += methodList->method_count;
      
      inversionList = [List create: getZone (self)];
      
      for (i = 0; i < methodList->method_count; i++)
        {
          aProbe = [MessageProbe createBegin: getZone (self)];
          [aProbe setProbedClass: probedClass];
          [aProbe setProbedSelector: methodList->method_list[i].method_name];
          if (objectToNotify != nil) 
            [aProbe setObjectToNotify: objectToNotify];
          aProbe = [aProbe createEnd];
          
          if(aProbe)
            [inversionList addFirst: aProbe];
          else
            numEntries--;
        }
      
      index = [inversionList begin: getZone (self)];
      while ((aProbe = [index next]))
        {
          [probes at: 
                    [String 
                      create: getZone (self)
                      setC: [aProbe getProbedMessage]] 
                  insert: 
                    aProbe];
          [index remove];
        }	
      [index drop];
      [inversionList drop];
    }

  return self;
}

PHASE(Using)

- clone: aZone
{
  ProbeMap *npm;
  id index;
  id aProbe;
  
  npm = [ProbeMap createBegin: aZone];
  [npm setProbedClass: probedClass];
  npm =	[npm _copyCreateEnd_];
  
  index = [self begin: aZone];
  
  while ((aProbe = [index next]) != nil)
    [npm _fastAddProbe_: [aProbe clone: aZone]];
  
  [index drop];
  
  return npm;
}

- (int)getNumEntries
{
  return numEntries;
}

- addProbeMap: (ProbeMap *)aProbeMap
{
  Class aClass;
  Class class;
  id index;
  id aProbe;
	
  aClass = [aProbeMap getProbedClass];

  for (class = probedClass; class != Nil; class = SD_SUPERCLASS (class))
    if (class == aClass)
      {
        index = [aProbeMap begin: globalZone];
        while ((aProbe = [index next]) != nil)
	  [self _fastAddProbe_: aProbe];
        [index drop];
        return self;
      }

  raiseEvent (WarningMessage,
              "ProbeMap not added because %s is not a superclass of %s\n",
              aClass->name, probedClass->name);

  return self;
}

- addProbe: aProbe
{
  id string;
  Class aClass;
  Class class;
	
  if([aProbe isKindOf: [VarProbe class]])
    string = [String create: getZone (self)
                           setC: [aProbe getProbedVariable]];
  else	
    string = [String create: getZone (self)
                           setC: STRDUP ([aProbe getProbedMessage])];
  
  if ([probes at: string] != nil)
    raiseEvent (WarningMessage,
                "addProbe: There was already a probe for %s!!!\n",
                [string getC]);
  
  aClass = [aProbe getProbedClass];
  
  for (class = probedClass;
       class != Nil;
       class = SD_SUPERCLASS (class))
    if (class == aClass)
      {
        [probes at: string insert: aProbe];
        numEntries++;
        if (objectToNotify != nil) 
          [aProbe setObjectToNotify: objectToNotify];
        return self;
      }
  
 raiseEvent (WarningMessage,
             "Probe not added to ProbeMap because %s is not a superclass of %s\n",
             aClass->name, probedClass->name);
  
  return self;
}


// Here, we don't check that the probe is of an appropriate class...
// This method is used by addProbeMap and clone where the check is done
// for all the probes within the candidate ProbeMap.
//
// Note: In practice, it is probably unnecessary to check for duplicate
//       inclusion...

- _fastAddProbe_: aProbe
{

  id string;

  if([aProbe isKindOf: [VarProbe class]])
    string = [String create: getZone (self)
                     setC: [aProbe getProbedVariable]];
  else
    string = [String create: getZone (self)
                     setC: STRDUP ([aProbe getProbedMessage])];

  if ([probes at: string] != nil)
    raiseEvent (WarningMessage,
                "addProbe: There was already a probe for %s!!!\n",
                [string getC]);

  [probes at: string insert: aProbe];
  numEntries++;

  if (objectToNotify != nil) 
    [aProbe setObjectToNotify: objectToNotify];

  return self;
}


// Note: the candidate ProbeMap for removal does not have to contain the
// *same* class in order to cause removal... Only probes with the same
// name!
//
// [We do not check that the classes are appropriate because the
// user may want to subtract commonly named methods from unrelated
// classes!!!]
- dropProbeMap: (ProbeMap *) aProbeMap
{
  id index;
  id aProbe;
			
  index = [aProbeMap begin: globalZone];

  while ((aProbe = [index next]) != nil)
    if ([aProbe isKindOf: [VarProbe class]])
      [self dropProbeForVariable: [aProbe getProbedVariable]];
    else
      [self dropProbeForMessage: STRDUP ([aProbe getProbedMessage])];
  
  [index drop];
	
  return self;
}

- dropProbeForVariable: (const char *)aVariable
{
  id string;
  
  string = [String create: getZone (self) setC: aVariable];
  if([probes removeKey: string] != nil)
    numEntries--;
  [string drop];
  
  return self;
}

- (Probe *)getProbeForVariable: (const char *)aVariable
{
  id string;
  id res;
  
  string = [String create: getZone (self) setC: aVariable];
  
  res = [probes at: string];
  [string drop];
  
  if (res == nil)
    { 
      // if not found
      if (SAFEPROBES)
        raiseEvent (WarningMessage,
                    "The variable %s was not found\n",
                    aVariable);
      return nil;
    }
  else
    return res;
}

- dropProbeForMessage: (const char *)aMessage
{
  id string;
  
  string = [String create: getZone (self) setC: aMessage];
  if([probes removeKey: string] != nil)
    numEntries--;
  [string drop];
  
  return self;
}

- (Probe *)getProbeForMessage: (const char *)aMessage
{
  id string;
  id res;
  
  string = [String create: getZone (self) setC: aMessage];

  res = [probes at: string];
  [string drop];

  if (res == nil)
    {
      if (SAFEPROBES)
        raiseEvent (WarningMessage,
                    "The message %s was not found\n",
                    aMessage);
      return nil;
    }
  else
    return res;
}

- begin: aZone
{
  return [probes begin: aZone];
}

- (void)describeForEach: stream
{
  id index = [probes begin: getCZone (getZone (self))];
  id member;

  for (member = [index next]; [index getLoc] == Member; member = [index next])
    xprint (member);
  [index drop];
}
@end
