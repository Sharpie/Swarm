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
