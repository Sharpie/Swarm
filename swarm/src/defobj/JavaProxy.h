// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <defobj/Create.h>

@interface JavaProxy: CreateDrop <Serialization>
- (BOOL)isJavaProxy;
- (void)createJavaCounterpart: (const char *)typeName;
@end
