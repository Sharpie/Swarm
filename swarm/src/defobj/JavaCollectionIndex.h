// Swarm library. Copyright © 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <defobj/Create.h>

@interface JavaCollectionIndex: CreateDrop
{
  unsigned count;
  int pos;
}
#ifdef HAVE_JDK
+ create: aZone setCount: (unsigned)count;
- (id <Symbol>)getLoc;
- next;
#endif
@end
