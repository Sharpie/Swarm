#import <SwarmEnvironment.h>

#import <simtools.h>
#import <collections.h>
#import <defobj.h>
#import <activity.h>
#import <random.h>
#import <objectbase.h>
#import <simtoolsgui.h>

#import <defobj/Customize.h>  // PHASE

@implementation SwarmEnvironment
PHASE(Creating)
+ (void)initSwarm: (const char *)appName
          version: (const char *)version
       bugAddress: (const char *)bugAddress
{
  _initSwarm_ (1, &appName, appName, version, bugAddress,
               Nil, NULL, NULL, NO, YES);
}
#include "SwarmEnvironment_getters.m"
@end
