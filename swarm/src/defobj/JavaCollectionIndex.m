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

+ create: aZone
{
  JavaCollectionIndex *obj = [super create: aZone];
  obj->status = Start;
  return obj;
}

- (id <Symbol>)getLoc
{
  return status;
}

- next
{
  jobject iterator = SD_JAVA_FIND_OBJECT_JAVA (self);
  jobject item;
  id proxy;

  if ((*jniEnv)->CallBooleanMethod (jniEnv, SD_JAVA_FIND_OBJECT_JAVA (self), m_IteratorHasNext))
    {
      item = (*jniEnv)->CallObjectMethod (jniEnv, iterator, m_IteratorNext);
      proxy = SD_JAVA_ENSUREOBJC (item);
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
