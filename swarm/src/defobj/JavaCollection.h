#import <defobj.h>
#import <defobj/Create.h>

@interface JavaCollection: CreateDrop
{
}
#ifdef HAVE_JDK
- (BOOL)isJavaCollection;
- (unsigned)getCount;
- begin: aZone;
- beginPermuted: aZone;
- getFirst;
- (void)forEach: (SEL)sel :arg1;
#endif
@end
