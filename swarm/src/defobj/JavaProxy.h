#import <defobj.h>
#import <defobj/Create.h>

@interface JavaProxy: CreateDrop
- (BOOL)isJavaProxy;
- createJavaCounterpart: (const char *)typeName;
@end
