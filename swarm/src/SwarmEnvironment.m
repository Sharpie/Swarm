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
+ (void)initSwarmArgc: (int)argc
                 argv: (const char **)argv
                 name: (const char *)appName
              version: (const char *)version
           bugAddress: (const char *)bugAddress
                class: (Class)class
{
  _initSwarm_ (argc, argv, appName, version, bugAddress,
               class, NULL, NULL, NO, YES);
}
#include "SwarmEnvironment_getters.m"
@end
