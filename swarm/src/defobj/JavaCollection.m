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

#ifdef HAVE_JDK
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

- begin: aZone
{
  jobject coll = SD_JAVA_FIND_OBJECT_JAVA (self);
  jclass class;
  jmethodID method;
  jobject iterator;
  ObjectEntry *entry;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, coll)))
    abort ();

  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"iterator",
				"()Ljava/util/Iterator;")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  iterator = (*jniEnv)->CallObjectMethod (jniEnv, coll, method);
  entry = SD_JAVA_ADD (iterator,
                       [JavaCollectionIndex create: getCZone (getZone (self))
                                            setCount: [self getCount]]);
  (*jniEnv)->DeleteLocalRef (jniEnv, iterator);
  return entry->object;
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
    [FArguments createBegin: getZone (self)];
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

  call = [FCall createBegin: getZone (self)];
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
  id index, member;
  BOOL ret = YES;
  jclass firstJavaClass;

  index = [(id) self begin: scratchZone];
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
    if (!member)
      {
        if (firstJavaClass)
          {
            ret = NO;
            break;
          }
      }
    else
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
  (*jniEnv)->DeleteLocalRef (jniEnv, firstJavaClass);
  [index drop];
  return ret;
}
#endif
@end
