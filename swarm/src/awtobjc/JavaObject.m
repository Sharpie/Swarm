// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/JavaEnv.h>
#import <awtobjc/JavaObject.h>
#import <awtobjc/common.h>
#import <objectbase/SwarmObject.h>

#include <misc.h>

#include <string.h>

@interface JavaArg: SwarmObject
{
  char type;
  const char *className;
  jvalue value;
}

+ createInt: aZone value: (jint)value;
+ createObj: aZone value: (jobject)value;
- (char)getValueType;
- (jint)getInt;
- (jobject)getJobject;
@end

@implementation JavaArg
+ createInt: aZone value: (jint)intValue
{
  JavaArg *arg = [super create: aZone];

  arg->type = 'I';
  arg->value.i = intValue;
  return arg;
}

+ createObj: aZone value: (jobject)objValue
{
  JavaArg *arg = [super create: aZone];

  arg->type = 'L';
  arg->value.l = objValue;
  return arg;
}

- (char)getValueType
{
  return type;
}

- (jint)getInt
{
  return value.i;
}

- (jobject)getJobject
{
  return value.l;
}

@end

@implementation JavaObject

+ createBegin: aZone
{
  JavaObject *obj = [super createBegin: aZone];

  if (javaEnv == 0)
    javaEnv = [JavaEnv new];

  obj->env = javaEnv;
  obj->jniEnv = [javaEnv getJNIEnv];
  obj->arguments = [List create: aZone];
  
  return obj;
}

- _setClassIdFromName_: (const char *)name prefix: (const char *)base
{
  char javaName[strlen (base) + strlen (name) + 1];

  strcpy (javaName, base);
  strcat (javaName, name);

  printf ("finding class: [%s]\n", javaName);
  classId = [env findClass: javaName];

  if (classId == 0)
    {
      fprintf (stderr, "Couldn't find Java class %s\n", name);
      exit (1);
    }
  return self;
}

- setClassIdFromName: (const char *)name
{
  return [self _setClassIdFromName_: name prefix: ""];
}

- setClassIdFromAWTName: (const char *)name
{
  return [self _setClassIdFromName_: name prefix: "java/awt/"];
}

- setClassIdFromSwarmName: (const char *)name
{
  return [self _setClassIdFromName_: name prefix: "swarm/"];
}

- addInt: (int)val
{
  [arguments addLast: [JavaArg createInt: [self getZone] value: val]];
  return self;
}

- _addJobject_: (jobject)jobject
{
  [arguments addLast: [JavaArg createObj: [self getZone] value: jobject]];
  return self;
}

- addObject: object
{
  printf ("adding: [%s] %p (%d)\n", [object name], [object getJobject], [arguments getCount]);
  [self _addJobject_: [object getJobject]];
  printf ("->(%d)\n", [arguments getCount]);
  return self;
}

- addString: (const char *)str
{
  [self _addJobject_: (*jniEnv)->NewStringUTF (jniEnv, str)];
  return self;
}

- (jclass)getJavaClass
{
  return (*jniEnv)->GetObjectClass (jniEnv, [self getJobject]);
}

- (const char *)getJavaClassNameFor: (jobject)object
{
  jclass objectClass, classClass;
  jobject classObject, nameObject;
  jmethodID getClassMethodID, getNameMethodID;

  objectClass = (*jniEnv)->GetObjectClass (jniEnv, object);
  getClassMethodID = findMethod (jniEnv, objectClass, "getClass", "()Ljava/lang/Class;");
  classObject = (*jniEnv)->CallObjectMethod (jniEnv, object, getClassMethodID);

  classClass = (*jniEnv)->GetObjectClass (jniEnv, classObject);
  getNameMethodID = findMethod (jniEnv, classClass, "getName", "()Ljava/lang/String;");
  nameObject = (*jniEnv)->CallObjectMethod (jniEnv, classObject, getNameMethodID);
  
  return (*jniEnv)->GetStringUTFChars (jniEnv, nameObject, NULL);
}

- createBackingObject
{
  int varCount = [arguments getCount], i;
  jvalue args[varCount];
  size_t len = 0;

  {
    id <Index> index = [arguments begin: scratchZone];
    
    for (i = 0; i < varCount; i++)
      {
        id argObj = [index next];
        
        if ([argObj getValueType] == 'L')
          len += 1 + strlen ([self getJavaClassNameFor: [argObj getJobject]]) + 1;
        else
          len++;
      }
    [index drop];
  }
  if (classId == NULL)
    [self setClassIdFromSwarmName: [self name]];
  
  {
    id <Index> index = [arguments begin: scratchZone];
    char signature_buf[1+len+1+1], *signature = signature_buf;

    *signature++ = '(';
    
    for (i = 0; i < varCount; i++)
      {
        id argObj = [index next];
        char type = [argObj getValueType];
        
        *signature++ = type;
        switch (type)
          {
          case 'I':
            args[i].i = [argObj getInt];
            break;
          case 'L': 
            args[i].l = [argObj getJobject];
            signature = stpcpy (signature, [self getJavaClassNameFor: [argObj getJobject]]);
            *signature++ = ';';
            break;
          default: abort ();
          }
      }
    *signature++ = ')';
    *signature++ = 'V';
    *signature = '\0';

    for (signature = signature_buf; *signature; signature++)
      if (*signature == '.')
        *signature = '/';

    [index drop];

    {
      jobject lobj;
      
      // Look up this flavor of constructor.
      jmethodID constrID;
      
      constrID = [self findMethod: "<init>" signature: signature_buf];
      
      if (constrID == 0)
        return 0;
      
      // Call the constructor.
      lobj = (*jniEnv)->NewObjectA (jniEnv, classId, constrID, args);
      if (lobj == NULL)
        abort ();
      
      printf ("CREATED lobj: %p for %s\n", (void *)lobj, [self name]);
      // Turn this into a global reference.
      jobj = (*jniEnv)->NewGlobalRef (jniEnv, lobj);
      printf ("jobj className: [%s]\n", [self getJavaClassNameFor: jobj]);
      (*jniEnv)->DeleteLocalRef (jniEnv, lobj);
      
      return self;
    }
  }
}

- createEnd
{
  return [self createBackingObject];
}

- (jobject)getJobject
{
  return jobj;
}

// Get the method id for a particular java method in this class.
- (jmethodID)findMethod: (const char *)methodName signature: (const char *)sig
{
  return findMethod (jniEnv, classId, methodName, sig);
}

// Get the method id for a particular java method in this class.
- (jfieldID)getFieldID: (const char *)field signature: (const char *)sig
{
  return getFieldID (jniEnv, classId, field, sig);
}

// Get the method id for a particular java method in this class.
- (jfieldID)getStaticFieldID: (const char *)field signature: (const char *)sig
{
  return getStaticFieldID (jniEnv, classId, field, sig);
}

// Retrieve field contents: for objects instanciated too briefly to get their 
// own objective C wrapper objects.
- (int)getIntField: (jobject)obj Field: (const char *)field
{
  return getIntField (jniEnv, obj, field);
}

// Retrieve static field contents.
- (int) getStaticIntField: (const char *)field
{
  return getStaticIntField (jniEnv, classId, field); 
}

// Get the method id for a particular <static> java method in this class.
- (jmethodID)findStaticMethod: (const char *)method signature: (const char *)sig
{
  jmethodID methodId = (*jniEnv)->GetStaticMethodID (jniEnv, classId, method, sig);

  if (methodId == 0)
    {
      fprintf (stderr, "warning: can't find static method %s\n", method);
      exit (1);
    }
  
  return methodId;
}

// Call a static java method; no args, returns object.
- (jobject)callObjectStaticMethod: (jmethodID)methodId
{
  jobject lobj, obj;
  
  if (methodId == 0)
    {
      fprintf (stderr, "callObjectStaticMethod: illegal method id\n");
      exit (1);
    }
  
  lobj = (*jniEnv)->CallStaticObjectMethod (jniEnv, classId, methodId);
  
  // only return global ref's to avoid nastiness
  obj = (*jniEnv)->NewGlobalRef (jniEnv, lobj);
  (*jniEnv)->DeleteLocalRef (jniEnv, lobj);
  
  return obj;
}

- (jint)callIntMethod: (jmethodID)methodId
{
  return (*jniEnv)->CallIntMethod (jniEnv, jobj, methodId);
}

- (void)callVoidMethod: (jmethodID)methodId
{
  // fprintf(stderr, "call void method 0\n");
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId);
}

- (void)callVoidMethod: (jmethodID)methodId: (int)val1
{
  // fprintf(stderr, "call void method 1, arg=%d\n", val1);
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, (jint)val1);
}

- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2
{
  fprintf(stderr, "call void method 2, arg=%d,%d\n", val1, val2);
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                                    (jint)val1, 
                                    (jint)val2);
}

- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2 
		       : (int)val3
{
  fprintf(stderr, "call void method 3, arg=%d,%d,%d\n", val1, val2, val3);
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                                    (jint)val1,
                                    (jint)val2, 
                                    (jint)val3);
}

- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2 
                      : (int)val3 : (int)val4 
{
  /* fprintf(stderr, "call void method 4, arg=%d,%d,%d,%d\n", 
                     val1,val2,val3,val4); */
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                                    (jint)val1, 
                                    (jint)val2, 
                                    (jint)val3, 
                                    (jint)val4);
}

- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2 
                      : (int)val3 : (int)val4 : (int)val5
{
  /* fprintf(stderr, "call void method 5, arg=%d,%d,%d,%d,%d\n", 
     val1, val2, val3, val4, val5); */
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                                    (jint)val1, 
                                    (jint)val2, 
                                    (jint)val3, 
                                    (jint)val4, 
                                    (jint)val5);
}

- (void)callVoidMethod: (jmethodID)methodId
                      O: (jobject)val1 : (int)val2 : (int)val3
{
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                                    (jobject)val1, 
                                    (jint)val2, 
                                    (jint)val3);
}


- (void)callVoidMethod: (jmethodID)methodId D: (double)val1 D: (double)val2
{
  return (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                                    (jdouble)val1, 
                                    (jdouble)val2);
}

- (void)callVoidMethod: (jmethodID)methodId
                     D: (double)val1 D: (double)val2
                     D: (double)val3 D: (double)val4
{
  (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId,
                             (jdouble)val1, (double)val2,
                             (jdouble)val3, (jdouble)val4);
}

- (void)callVoidMethod: (jmethodID)methodId S: (const char *)val1
{
  jstring str1 = (*jniEnv)->NewStringUTF (jniEnv, val1);
  
  printf ("Calling: %p %p %p %p\n", jniEnv, jobj, methodId, str1);
  (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, str1); 
  printf ("Done\n");
  (*jniEnv)->DeleteLocalRef (jniEnv, str1);
}

- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 S: (const char *)val2
{
  jstring str1 = (*jniEnv)->NewStringUTF (jniEnv, val1);
  jstring str2 = (*jniEnv)->NewStringUTF (jniEnv, val2);
  
  (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, str1, str2);
  (*jniEnv)->DeleteLocalRef (jniEnv, str1);
  (*jniEnv)->DeleteLocalRef (jniEnv, str2);
}

- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 I: (int)val2
{
  jstring str1 = (*jniEnv)->NewStringUTF (jniEnv, val1);
  
  (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, str1, (jint)val2);
  (*jniEnv)->DeleteLocalRef (jniEnv, str1);
}

- (void) callVoidMethod: (jmethodID)methodId
                      S: (const char *)val1 
		      D: (double)val2
                      D: (double)val3
{
  jstring str1 = (*jniEnv)->NewStringUTF (jniEnv, val1);

  (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId, 
                             str1, (jdouble)val2, (jdouble)val3);
  
  (*jniEnv)->DeleteLocalRef (jniEnv, str1);
}

- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 
                     S: (const char *)val2
                     D: (double)val3 
{
  jstring str1 = (*jniEnv)->NewStringUTF (jniEnv, val1);
  jstring str2 = (*jniEnv)->NewStringUTF (jniEnv, val2);
  
  fprintf (stderr, "calling 2sD void with %s\n", val1);
  (*jniEnv)->CallVoidMethod (jniEnv, jobj, methodId,
                             str1, str2, (jdouble)val3);
  
  (*jniEnv)->DeleteLocalRef (jniEnv, str1);
  (*jniEnv)->DeleteLocalRef (jniEnv, str2);
}

- (jobject)callObjectMethod: (jmethodID)methodId
{
  return (*jniEnv)->CallObjectMethod (jniEnv, jobj, methodId);
}

- (jobject)callObjectMethod: (jmethodID)methodId O: (jobject)arg1
{
  return (*jniEnv)->CallObjectMethod (jniEnv, jobj, methodId, arg1);
}

@end
