// Swarm library. Copyright © 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/JavaCollection.h>

#ifdef HAVE_JDK
#import <defobj/directory.h>
#import <defobj/defalloc.h> // getZone
#import <defobj/FCall.h>
#import <collections.h> // Member
#import <defobj/JavaCollectionIndex.h>
#import <collections/Permutation.h>
#import "java.h"
#endif

@implementation JavaCollection
PHASE(Creating)
PHASE(Setting)

#ifdef HAVE_JDK

- lispIn: expr
{
  id index, member;

  index = [(id) expr begin: scratchZone];
  for (member = [index next]; [index getLoc] == Member; member = [index next])
    [(id) self addLast: lispIn ([self getZone], member)];
  [index drop];
  return self;
}

#include "../collections/List_HDF5in.m"

PHASE(Using)

- (BOOL)isJavaCollection
{
  return YES;
}

- (unsigned)getCount
{
  jobject coll = SD_JAVA_FIND_OBJECT_JAVA (self);
  jclass class;
  jmethodID method;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, coll)))
    abort ();

  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"size",
				"()I")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  return (*jniEnv)->CallIntMethod (jniEnv, coll, method);
}

- (void)addLast: anObject
{
  // do the lookup here since Collection is an interface
  // and Kaffe has problems caching method ids from interfaces
  jobject coll = SD_JAVA_FIND_OBJECT_JAVA (self);
  jclass class;
  jmethodID method;
  
  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, coll)))
    abort ();
  
  if (!(method =
        (*jniEnv)->GetMethodID (jniEnv,
                                class,
                                "add",
                                "(Ljava/lang/Object;)Z")))
    abort ();
  (*jniEnv)->CallBooleanMethod (jniEnv, coll, method,
                                SD_JAVA_ENSUREJAVA (anObject));
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
}

- begin: aZone
{
  jobject coll = SD_JAVA_FIND_OBJECT_JAVA (self);
  jclass class;
  jmethodID method;
  jobject lref;
  id iteratorProxy;
  
  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, coll)))
    abort ();

  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"iterator",
				"()Ljava/util/Iterator;")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  lref = (*jniEnv)->CallObjectMethod (jniEnv, coll, method);
  iteratorProxy = [JavaCollectionIndex create: aZone
                                       setIterator: lref
                                       setCount: [self getCount]];
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  return iteratorProxy;
}

- beginPermuted: aZone
{
  return [[[PermutedIndex_c createBegin: aZone]
            setCollection: self] createEnd];
}

- getFirst
{
  jobject coll = SD_JAVA_FIND_OBJECT_JAVA (self);
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, coll);
  jobject first;
  jmethodID method;
  id ret;

  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"get",
				"(I)Ljava/lang/Object;")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  first = (*jniEnv)->CallObjectMethod (jniEnv, coll, method, 0);
  ret = SD_JAVA_ENSUREOBJC (first);
  (*jniEnv)->DeleteLocalRef (jniEnv, first);
  return ret;
}

- (void)forEach: (SEL)sel :arg1
{
  id <FCall> call;
  id <FArguments> arguments =
    [FArguments createBegin: getCZone (getZone (self))];
  jobject jsel = SD_JAVA_FIND_SELECTOR_JAVA (sel);
  jobject coll = SD_JAVA_FIND_OBJECT_JAVA (self);
  const char *sig = java_ensure_selector_type_signature (jsel);
  jobject javaTarget;
  jsize i, size;
  jclass class;
  jmethodID method;
  char *selname, *ptr;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, coll)))
    abort ();
  
  [arguments setJavaSignature: sig];
  [arguments setObjCReturnType: _C_ID];
  [arguments addObject: arg1];
  arguments = [arguments createEnd];

  call = [FCall createBegin: getCZone (getZone (self))];
  [call setArguments: arguments];

  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"size",
				"()I")))
    abort ();
  size = (*jniEnv)->CallIntMethod (jniEnv, coll, method);
  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"get",
				"(I)Ljava/lang/Object;")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  javaTarget = (*jniEnv)->CallObjectMethod (jniEnv, coll, method, 0);
  selname = SSTRDUP (sel_get_name (sel));
  ptr = strchr (selname, ':');
  *ptr = '\0';
  [call setJavaMethod: selname inObject: javaTarget];
  call = [call createEnd];
  [call performCall];
  (*jniEnv)->DeleteLocalRef (jniEnv, javaTarget);
  for (i = 1; i < size; i++)
    {
      javaTarget = (*jniEnv)->CallObjectMethod (jniEnv, coll, method, i);
      updateJavaTarget (call, javaTarget);
      [call performCall];
      (*jniEnv)->DeleteLocalRef (jniEnv, javaTarget);
    }
  [call drop]; 
  [arguments drop];
  [scratchZone free: (void *) selname];
  [scratchZone free: (void *) sig];
}

- (BOOL)allSameClass
{
  id index  = [(id) self begin: scratchZone];
  BOOL ret = YES;
  
  jclass firstJavaClass;
  id member;

  member = [index next];
  
  if (member)
    {
      jobject firstJavaObject = SD_JAVA_FIND_OBJECT_JAVA (member);
      
      firstJavaClass = (*jniEnv)->GetObjectClass (jniEnv, firstJavaObject);
    }
  else
    firstJavaClass = (jclass) 0;
    
  for (member = [index next];
       [index getLoc] == Member;
       member = [index next])
    if ((!member && firstJavaClass)
        || (!firstJavaClass && member))
      {
        ret = NO;
        break;
      }
    else if (member)
      {
        jobject javaObject = SD_JAVA_FIND_OBJECT_JAVA (member);
        jclass javaClass = (*jniEnv)->GetObjectClass (jniEnv, javaObject);

        ret = (((*jniEnv)->IsSameObject (jniEnv, javaClass, firstJavaClass)
                == JNI_TRUE)
               ? YES
               : NO);
        (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
        if (ret == NO)
          break;
      }
  if (firstJavaClass)
    (*jniEnv)->DeleteLocalRef (jniEnv, firstJavaClass);
  [index drop];
  return ret;
}

- (void)_lispOut_: outputCharStream deep: (BOOL)deepFlag
{
  id index, member;

  [outputCharStream catStartMakeInstance: [self getTypeName]];
  [outputCharStream catSeparator];
  index = [(id) self begin: getCZone (getZone (self))];
  if (deepFlag)
    {
      for (member = [index next];
           [index getLoc] == Member; 
           member = [index next])
        if (member)
          [member lispOutDeep: outputCharStream];
    }
  else
    {
      for (member = [index next];
           [index getLoc] == Member; 
           member = [index next])
        if (member)
          [member lispOutShallow: outputCharStream];
    }
  [index drop];
  [outputCharStream catEndExpr];
}

- (void)lispOutDeep: stream
{
  [self _lispOut_: stream deep: YES];
}

- (void)lispOutShallow: stream
{
  [self _lispOut_: stream deep: NO];
}

#include "../collections/List_HDF5out.m"

#else
PHASE(Using)
#endif
@end
