#import <defobj.h>
#import <defobj/Create.h>

@interface JavaCollection: CreateDrop
{
}
- (BOOL)isJavaCollection;
- (unsigned)getCount;
- begin: aZone;
- getFirst;
- (void)forEach: (SEL)sel :arg1;
@end
