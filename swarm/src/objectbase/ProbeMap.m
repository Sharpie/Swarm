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
#import "../defobj/java.h" // SD_JAVA_ENSURE_SELECTOR_OBJC, SD_JAVA_FIND_OBJECT_JAVA, java_field_usable_p
#import "../defobj/COM.h" // SD_COM_FIND_CLASS_COM, COM_collect_methods
#import "../defobj/javavars.h" // m_*, c_*
#endif

@implementation ProbeMap
PHASE(Creating)
+ createBegin: aZone
{
  ProbeMap *tempObj;

  tempObj = [super createBegin: aZone];
  tempObj->objectToNotify = nil;
  tempObj->count = 0;
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
  count = 0;
  
  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;
  
  return self;
}

- (void)_addVarProbe_: (Class)aClass variableName: (const char *)name
{
  id <VarProbe> varProbe = [VarProbe createBegin: getZone (self)];
  id <String> key = [String create: getZone (self) setC: name];

  [varProbe setProbedClass: aClass];
  [varProbe setProbedVariable: name];
	  
  if (objectToNotify != nil) 
    [varProbe setObjectToNotify: objectToNotify];
  varProbe = [varProbe createEnd];

  if (!varProbe || ![probes at: key insert: varProbe])
    {
      if (varProbe)
        [varProbe drop];
      [key drop];
    }
  else
    count++;
}

- (void)_addMessageProbe_: (Class)aClass selector: (SEL)aSel
{
  id <MessageProbe> messageProbe = [MessageProbe createBegin: getZone (self)];
  id <String> key;
  
  [messageProbe setProbedClass: aClass];
  [messageProbe setProbedSelector: aSel];
  if (objectToNotify != nil) 
    [messageProbe setObjectToNotify: objectToNotify];
  messageProbe = [messageProbe createEnd];

  key = [String create: getZone (self) setC: [messageProbe getProbedMessage]];
  
  if (!messageProbe || ![probes at: key insert: messageProbe])
    {
      if (messageProbe)
        [messageProbe drop];
      [key drop];
    }
  else
    count ++;
}


#ifdef HAVE_JDK

- (void)addJavaFields: (jclass)javaClass
{
  jarray fields;
  jsize fieldCount;

  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv,
                                              javaClass,
                                              m_ClassGetDeclaredFields)))
    abort();

  fieldCount = (*jniEnv)->GetArrayLength (jniEnv, fields);

  while (fieldCount > 0)
    {
      jobject field;
      
      fieldCount--;
      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, fieldCount);
      if (java_field_usable_p (field))
	{
	  jstring name;
	  const char *buf;
	  jboolean isCopy;

          name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
	  buf = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
          [self _addVarProbe_: probedClass variableName: buf];
	  
	  if (isCopy)
	    (*jniEnv)->ReleaseStringUTFChars (jniEnv, name, buf);
	  (*jniEnv)->DeleteLocalRef (jniEnv, name);
	}
      (*jniEnv)->DeleteLocalRef (jniEnv, field);
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, fields);
}

- (void)addJavaMethods: (jclass)javaClass
{
  jarray methods;
  jsize methodCount;

  if (!(methods = (*jniEnv)->CallObjectMethod (jniEnv,
                                               javaClass,
                                               m_ClassGetDeclaredMethods)))
    abort();
  
  methodCount = (*jniEnv)->GetArrayLength (jniEnv, methods);
  
  if (methodCount)
    {
      while (methodCount > 0)
        {
	  jobject method;
          
          methodCount--;
            
	  method = (*jniEnv)->GetObjectArrayElement (jniEnv,
                                                     methods,
                                                     methodCount);
	  if (java_method_usable_p (method))
	    {
              SEL sel;
              jobject selector;
	      jstring name = (*jniEnv)->CallObjectMethod (jniEnv,
							  method, 
							  m_MethodGetName);

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
                  sel = SD_JAVA_ENSURE_SELECTOR_OBJC (selector);
                  (*jniEnv)->DeleteLocalRef (jniEnv, selector);

                  [self _addMessageProbe_: probedClass selector: sel];
                }
            }
	  (*jniEnv)->DeleteLocalRef (jniEnv, method);
        }
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, methods);
}
#endif

- (void)addObjcFields: (Class)aClass
{ 
  IvarList_t ivarList;

  if ((ivarList = aClass->ivars))
    {
      unsigned i;
      
      for (i = 0; i < ivarList->ivar_count; i++)
          [self _addVarProbe_: aClass
                variableName: ivarList->ivar_list[i].ivar_name];
    }
}

- (void)addObjcMethods: (Class)aClass
{
  MethodList_t methodList;

  if ((methodList = aClass->methods))
    {
      unsigned i;
      
      for (i = methodList->method_count; i > 0; i--)
        [self _addMessageProbe_: aClass
              selector: methodList->method_list[i - 1].method_name];
    }
}

- (void)addCOMFields: (Class)aClass
{
  COMclass cClass = SD_COM_FIND_CLASS_COM (aClass);
  
  void collect_variable (COMmethod getterMethod, COMmethod setterMethod)
    {
      printf ("variable: `%s'\n", COM_method_name (getterMethod));
      // [self _addVarProbe_: aClass variableName: name];
    }

  if (!cClass)
    abort ();

  COM_collect_variables (cClass, collect_variable);
}

- (void)addCOMMethods: (Class)aClass
{
  COMclass cClass = SD_COM_FIND_CLASS_COM (aClass);

  void collect_method (COMmethod method)
    {
      // [self _addMessageProbe_: aClass selector: ??
      printf ("method: `%s'\n", COM_method_name (method));
    }

  if (!cClass)
    abort ();
  
  COM_collect_methods (cClass, collect_method);
}

  
- createEnd
{
  if (SAFEPROBES)
    if (!probedClass)
      {
        raiseEvent (WarningMessage,
                    "ProbeMap object was not properly initialized\n");
        return nil;
      }

  if (!objectToNotify)
    [self setObjectToNotify: 
            [probeLibrary getObjectToNotify]];
  
  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];  

#ifdef HAVE_JDK
  if ([probedClass respondsTo: M(isJavaProxy)])
    { 
      jclass classObject = SD_JAVA_FIND_CLASS_JAVA (probedClass);

      [self addJavaFields: classObject];
      [self addJavaMethods: classObject];
      return self;
    }
#endif
  else if (COM_init_p () && SD_COM_FIND_CLASS_COM (probedClass))
    {
      [self addCOMFields: probedClass];
      [self addCOMMethods: probedClass];
    }
  else
    {
      [self addObjcFields: probedClass];
      [self addObjcMethods: probedClass];
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

- (unsigned)getCount
{
  return count;
}

- addProbeMap: (id <ProbeMap>)aProbeMap
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

- addProbe: (id <Probe>)aProbe
{
  id <String> string;
  Class aClass, class;
	
  if ([aProbe conformsTo: @protocol (VarProbe)])
    string = [String create: getZone (self)
                     setC: [(id <VarProbe>)aProbe getProbedVariable]];
  else	
    string = [String create: getZone (self)
                     setC: STRDUP ([(id <MessageProbe>) aProbe getProbedMessage])];
  
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
        count++;
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

- _fastAddProbe_: (id <Probe>)aProbe
{

  id string;

  if ([aProbe conformsTo: @protocol (VarProbe)])
    string = [String create: getZone (self)
                     setC: [(id <VarProbe>) aProbe getProbedVariable]];
  else
    string = [String create: getZone (self)
                     setC: STRDUP ([(id <MessageProbe>) aProbe getProbedMessage])];

  if ([probes at: string] != nil)
    raiseEvent (WarningMessage,
                "addProbe: There was already a probe for %s!!!\n",
                [string getC]);

  [probes at: string insert: aProbe];
  count++;

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
- dropProbeMap: (id <ProbeMap>)aProbeMap
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
    count--;
  [string drop];
  
  return self;
}

- (id <VarProbe>)getProbeForVariable: (const char *)aVariable
{
  id string;
  id res;
  
  string = [String create: getZone (self) setC: aVariable];
  
  res = [probes at: string];
  [string drop];
  
  return res;
}

- dropProbeForMessage: (const char *)aMessage
{
  id string;
  
  string = [String create: getZone (self) setC: aMessage];
  if([probes removeKey: string] != nil)
    count--;
  [string drop];
  
  return self;
}

- (id <Probe>)getProbeForMessage: (const char *)aMessage
{
  id string;
  id res;
  
  string = [String create: getZone (self) setC: aMessage];

  res = [probes at: string];
  [string drop];

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
