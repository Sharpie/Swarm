#import <defobj.h>
#import <defobj/Create.h>

@interface JavaCollectionIndex: CreateDrop
{
}
#ifdef HAVE_JDK
- (id <Symbol>)getLoc;
- next;
#endif
@end
