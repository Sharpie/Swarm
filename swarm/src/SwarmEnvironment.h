#import <swarm.h>

#import <defobj.h>
#import <collections.h>
#import <objectbase.h>
#import <random.h>
#import <simtools.h>
#import <simtoolsgui.h>

@interface SwarmEnvironment: Object <SwarmEnvironment>
{
}
+ (void)initSwarm: (const char *)appName
          version: (const char *)version
       bugAddress: (const char *)bugAddress;

#import "SwarmEnvironment_getters.h"
@end
