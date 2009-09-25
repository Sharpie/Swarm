// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <objectbase/ProbeMap.h>
#import <defobj/swarm-objc-api.h>
#import <defobj.h> // Warning, STRDUP
#import <defobj/defalloc.h> // getZone
#import <objectbase/VarProbe.h>
#import <defobj/macros.h> // DROP
#import <collections/Map.h> // at:keySlot:memberSlot:

#import "local.h"

#include <swarmconfig.h> // HAVE_JDK
#import <defobj/directory.h> // SD_SUPERCLASS, SD_GETCLASS
#ifdef HAVE_JDK
#import "../defobj/java.h" // SD_JAVA_ENSURE_SELECTOR_OBJC, SD_JAVA_FIND_OBJECT_JAVA, java_field_usable_p
#import "../defobj/javavars.h" // m_*, c_*
#endif
#import "../defobj/COM.h" // SD_COM_ENSURE_CLASS_COM, COM_collect_methods

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

- setProbedObject: anObject
{
  COMobject cObj = SD_COM_FIND_OBJECT_COM (anObject);

  probedObject = anObject;
  
  if (cObj && COM_is_javascript (cObj))
    probedClass = Nil;
  else
    [self setProbedClass: SD_GETCLASS (anObject)];
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

- (void)_finishVarProbe_: (id <VarProbe>)varProbe
{
  if (objectToNotify != nil) 
    [varProbe setObjectToNotify: objectToNotify];

  varProbe = [varProbe createEnd];
  if (varProbe)
    {
      id <String> key = [String create: getZone (self) setC: 
                                  [varProbe getProbedVariable]];
      
      if (![probes at: key insert: varProbe])
        {
          [varProbe drop];
          [key drop];
        }
      else
        count++;
    }
}

- (void)_addVarProbeForClass_: (Class)aClass variableName: (const char *)name
{
  id <VarProbe> varProbe = [VarProbe createBegin: getZone (self)];

  [varProbe setProbedClass: aClass];
  [varProbe setProbedVariable: name];

  [self _finishVarProbe_: varProbe];
}

- (void)_addVarProbeForObject_: object variableName: (const char *)name
{
  id <VarProbe> varProbe = [VarProbe createBegin: getZone (self)];

  [varProbe setProbedObject: object];
  [varProbe setProbedVariable: name];
	  
  [self _finishVarProbe_: varProbe];
}

- (void)_addVarProbe_: (COMclass)cClass
               getter: (COMmethod)getter
               setter: (COMmethod)setter
{
  VarProbe *varProbe = [VarProbe createBegin: getZone (self)];
  
  [varProbe setProbedVariable: COM_method_name (getter)];
  [varProbe setProbedClass: SD_COM_ENSURE_CLASS_OBJC (cClass)];
  [varProbe setProbedCOMgetter: getter setter: setter];

  [self _finishVarProbe_: varProbe];
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
    count++;
}

- (void)_addMessageProbe_: obj methodName: (const char *)methodName
{
  id <MessageProbe> messageProbe = [MessageProbe createBegin: getZone (self)];
  id <String> key;
  
  [messageProbe setProbedObject: obj];
  [messageProbe setProbedMethodName: methodName];
  if (objectToNotify != nil) 
    [messageProbe setObjectToNotify: objectToNotify];
  messageProbe = [messageProbe createEnd];

  key = [String create: getZone (self) setC: methodName];
  
  if (!messageProbe || ![probes at: key insert: messageProbe])
    {
      if (messageProbe)
        [messageProbe drop];
      [key drop];
    }
  else
    count++;
}


#ifdef HAVE_JDK

- (void)addJavaFields: (jclass)javaClass
{
  jarray fields;
  jsize fieldCount, i;

  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv,
                                              javaClass,
                                              m_ClassGetDeclaredFields)))
    abort();

  fieldCount = (*jniEnv)->GetArrayLength (jniEnv, fields);

  for (i = 0; i < fieldCount; i++)
    {
      jobject field;
      
      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, i);
      if (java_field_usable_p (field))
	{
	  jstring name;
	  const char *buf;
	  jboolean isCopy;

          name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
	  buf = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
          [self _addVarProbeForClass_: probedClass variableName: buf];
	  
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
      jsize i;

      for (i = 0; i < methodCount; i++)
        {
	  jobject method;
          
	  method = (*jniEnv)->GetObjectArrayElement (jniEnv, methods, i);

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
#if SWARM_OBJC_DONE
  IvarList_t ivarList;

  if ((ivarList = aClass->ivars))
    {
      int i;
      
      for (i = 0; i < ivarList->ivar_count; i++)
	[self _addVarProbeForClass_: aClass
	      variableName: ivarList->ivar_list[i].ivar_name];
    }
#else
  unsigned i, outCount;
  ObjcIvar *ivarList = swarm_class_copyIvarList(aClass, &outCount);
  for (i = 0; i < outCount; ++i) {
    [self _addVarProbeForClass_: aClass
	  variableName: swarm_ivar_getName(ivarList[i])];
  }
  if (ivarList) free(ivarList);
#endif
}

- (void)addObjcMethods: (Class)aClass
{
#if SWARM_OBJC_DONE
  MethodList_t methodList;

  if ((methodList = aClass->methods))
    {
      unsigned i;
      
      for (i = methodList->method_count; i > 0; i--)
        [self _addMessageProbe_: aClass
              selector: methodList->method_list[i - 1].method_name];
    }
#else
  unsigned i, outCount;
  ObjcMethod *methodList = swarm_class_copyMethodList(aClass, &outCount);
  for (i = 0; i < outCount; ++i)
    [self _addMessageProbe_: aClass
	  selector: swarm_method_getName(methodList[i])];
  if (methodList) free(methodList);
#endif
}

- (void)addCOMFields: (COMclass)cClass
{
  void collect_variable (COMmethod getterMethod, COMmethod setterMethod)
    {
      [self _addVarProbe_: cClass
            getter: getterMethod
            setter: setterMethod];
    }

  if (!cClass)
    abort ();

  COM_collect_variables (cClass, collect_variable);
}

- (void)addCOMMethods: (COMclass)cClass
{
  void collect_method (COMmethod method)
    {
      COMselector cSel = COM_selector_create (method);
         
      [self _addMessageProbe_: SD_COM_ENSURE_CLASS_OBJC (cClass)
            selector: SD_COM_ENSURE_SELECTOR_OBJC (cSel)];
    }

  if (!cClass)
    abort ();
  
  COM_collect_methods (cClass, collect_method);
}

- (void)addJSFields: (COMobject)cObj
{
  void collect (const char *variableName)
    {
      [self _addVarProbeForObject_: SD_COM_ENSURE_OBJECT_OBJC (cObj)
            variableName: variableName];
    }
  JS_collect_variables (cObj, collect);
}

- (void)addJSMethods: (COMobject)cObj
{
  void collect (const char *methodName)
    {
      [self _addMessageProbe_: SD_COM_ENSURE_OBJECT_OBJC (cObj)
            methodName: methodName];
    }
  JS_collect_methods (cObj, collect);
}
  
- createEnd
{
  if (SAFEPROBES)
    if (!probedClass && !probedObject)
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

  if (COM_init_p ())
    {
      if (probedClass)
        {
          COMclass cClass = SD_COM_FIND_CLASS_COM (probedClass);
          
          if (cClass)
            {
              [self addCOMFields: cClass];
              [self addCOMMethods: cClass];
              return self;
            }
        }
      else if (probedObject)
        {
          COMobject cObj = SD_COM_FIND_OBJECT_COM (probedObject);

          if (cObj)
            {
              if (COM_is_javascript (cObj))
                {
                  [self addJSFields: cObj];
                  [self addJSMethods: cObj];
                  return self;
                }
              else
                abort ();
            }
          else
            abort ();
        }
    }
  // note need for drop-thru above
#ifdef HAVE_JDK
  if ([probedClass respondsTo: M(isJavaProxy)])
    { 
      jclass classObject = SD_JAVA_FIND_CLASS_JAVA (probedClass);

      [self addJavaFields: classObject];
      [self addJavaMethods: classObject];
    }
#endif
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
              swarm_class_getName(aClass), swarm_class_getName(probedClass));

  return self;
}

- addProbe: (id <Probe>)aProbe
{
  id <String> string;
  Class aClass, class;

  if (!aProbe)
    raiseEvent (WarningMessage,
                "Attempt to add null probe\n");
  else
    {
      if ([aProbe conformsTo: @protocol (VarProbe)])
        string = [String create: getZone (self)
                         setC: [(id <VarProbe>) aProbe getProbedVariable]];
      else	
        string = [String create: getZone (self)
                         setC: [(id <MessageProbe>) aProbe getProbedMessage]];
      
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
                  swarm_class_getName(aClass), swarm_class_getName(probedClass));
    }
      
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
                     setC: ([(id <MessageProbe>) aProbe getProbedMessage])];

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
      [self dropProbeForMessage: [aProbe getProbedMessage]];
  
  [index drop];
	
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

- (void)dropProbeFor: (const char *)aStr
{
  id string;
  id *keyPtr, *memberPtr, key;
  
  string = [String create: getZone (self) setC: aStr];

  if (![probes at: string])
    abort ();

  if ([probes at: string keySlot: &keyPtr memberSlot: &memberPtr])
    abort ();
  
  key = *keyPtr;
  [probes removeKey: string];
  count--;
  DROP (string);
  DROP (key);
}

- (void)dropProbeForMessage: (const char *)aMessage
{
  [self dropProbeFor: aMessage];
}

- (void)dropProbeForVariable: (const char *)aVariable
{
  [self dropProbeFor: aVariable];
}

- (id <MessageProbe>)getProbeForMessage: (const char *)aMessage
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

- (void)drop
{
  [probes drop];
  [super drop];
}
@end
