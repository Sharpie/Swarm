// Swarm library. Copyright © 2000 Swarm Development Group.
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

#import <defobj/JavaCollectionIndex.h>

#include <swarmconfig.h>

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
  
  obj->iterator = SD_JAVA_ADD_OBJECT_JAVA (lref, obj);
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
      id proxy = SD_JAVA_ENSURE_OBJECT_OBJC (item);

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
