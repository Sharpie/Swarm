// Swarm library. Copyright © 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "JavaProxy.h"
#include <swarmconfig.h>

@interface JavaCollection: JavaProxy
{
}
#ifdef HAVE_JDK
- (BOOL)isJavaCollection;
- (unsigned)getCount;
- begin: aZone;
- beginPermuted: aZone;
- getFirst;
- (void)addLast: obj;
- (void)forEach: (SEL)sel :arg1;
- lispIn: expr;
- (void)lispOutDeep: stream;
- (void)lispOutShallow: stream;
- hdf5In: hdf5Obj;
- (void)hdf5OutDeep: hdf5Obj;
- (void)hdf5OutShallow: hdf5Obj;
#endif
@end
