#import <defobj.h>
#import <defobj/Create.h>

@interface JavaCollectionIndex: CreateDrop
{
  size_t count;
  int pos;
}
#ifdef HAVE_JDK
+ create: aZone setCount: (size_t)count;
- (id <Symbol>)getLoc;
- next;
#endif
@end
