#include <objc/Object.h>

@interface JavaProxy: Object
- (retval_t)forward: (SEL)aSel : (arglist_t)argFrame;
@end
