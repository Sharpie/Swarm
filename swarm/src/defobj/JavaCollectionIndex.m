#import <defobj/JavaCollectionIndex.h>

#ifdef HAVE_JDK
#import <defobj/directory.h>
#import <collections.h> // Member, End
#endif

@implementation JavaCollectionIndex

#ifdef HAVE_JDK
+ create: aZone setCount: (unsigned)theCount
{
  JavaCollectionIndex *obj = [self create: aZone];

  obj->pos = -1;
  obj->count = theCount;
  return obj;
}


- (id <Symbol>)getLoc
{
  return pos < count ? Member : End;
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
  pos++;
  return proxy;
}
#endif

@end
