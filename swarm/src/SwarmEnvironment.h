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
+ (void)initSwarmArgc: (int)argc
                 argv: (const char **)argv
                 name: (const char *)appName
              version: (const char *)version
           bugAddress: (const char *)bugAddress
                class: (Class)class;

#import "SwarmEnvironment_getters.h"
@end
