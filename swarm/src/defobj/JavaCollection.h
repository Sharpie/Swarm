#import <defobj.h>
#import <defobj/Create.h>

@interface JavaCollection: CreateDrop
{
}
- (BOOL)isJavaProxy;
- (unsigned)getCount;
- begin: aZone;
- (id <Symbol>)getLoc;
- next;
- getFirst;
- (void)forEach: (SEL)sel :arg1;
@end
