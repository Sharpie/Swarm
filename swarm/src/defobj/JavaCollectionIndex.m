#import <defobj/directory.h>
#import <collections.h> // Member, End
#import <defobj/JavaCollectionIndex.h>

@implementation JavaCollectionIndex

- (id <Symbol>)getLoc
{
  jobject iterator = SD_FINDJAVA (jniEnv, self);
  jclass class;
  jmethodID method;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, iterator)))
    abort (); 
  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"hasNext",
				"()Z")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  return ((*jniEnv)->CallBooleanMethod (jniEnv, iterator, method) == JNI_TRUE
	  ? Member
	  : End);
}

- next
{
  jobject iterator = SD_FINDJAVA (jniEnv, self);
  jobject item;
  id proxy;
  jclass class;
  jmethodID method;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, iterator)))
    abort ();
  if (!(method =
	(*jniEnv)->GetMethodID (jniEnv,
				class,
				"next",
				"()Ljava/lang/Object;")))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  item = (*jniEnv)->CallObjectMethod (jniEnv, iterator, method);
  proxy = SD_ENSUREOBJC (jniEnv, item);
  (*jniEnv)->DeleteLocalRef (jniEnv, item);
  return proxy;
}

@end
