// Swarm library. Copyright © 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/JavaCollectionIndex.h>

#ifdef HAVE_JDK
#import <defobj/directory.h>
#import <collections.h> // Member, End
#import "java.h"
#import "javavars.h"
#endif

@implementation JavaCollectionIndex

#ifdef HAVE_JDK

+ create: aZone setIterator: (jobject)lref setCount: (unsigned)theCount
{
  JavaCollectionIndex *obj = [super create: aZone];
  jclass clazz = (*jniEnv)->GetObjectClass (jniEnv, lref);
  
  obj->status = Start;
  
  if (!(obj->m_next =
        (*jniEnv)->GetMethodID (jniEnv, clazz, "next", "()Ljava/lang/Object;")))
    abort ();
  if (!(obj->m_hasNext =
        (*jniEnv)->GetMethodID (jniEnv, clazz, "hasNext", "()Z")))
    abort ();
  
  obj->iterator = SD_JAVA_ADDJAVA (lref, obj);
  return obj;
}

- (id <Symbol>)getLoc
{
  return status;
}

- next
{
  if ((*jniEnv)->CallBooleanMethod (jniEnv, iterator, m_hasNext))
    {
      jobject item = (*jniEnv)->CallObjectMethod (jniEnv, iterator, m_next);
      id proxy = SD_JAVA_ENSUREOBJC (item);

      if (item)
        (*jniEnv)->DeleteLocalRef (jniEnv, item);
      status = Member;
      return proxy;
    }
  else
    {
      status = End;
      return nil;
    }
}

#endif

@end
