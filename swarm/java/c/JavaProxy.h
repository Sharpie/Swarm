#import <defobj.h>
#import <defobj/Create.h>

@interface JavaProxy: CreateDrop
- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame;
@end
