// Swarm library. Copyright © 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <defobj/Create.h>
#import <swarmconfig.h>
#import "java.h"

@interface JavaCollectionIndex: CreateDrop
{
#ifdef HAVE_JDK
  jobject iterator;
  jmethodID m_next, m_hasNext;
#endif
  id <Symbol> status;
}
#ifdef HAVE_JDK
+ create: aZone setIterator: (jobject)lref setCount: (unsigned)count;
- (id <Symbol>)getLoc;
- next;
#endif
@end
