#import <swarm.h>

#import <defobj.h>
#import <collections.h>
#import <objectbase.h>
#import <random.h>
#import <simtools.h>
#import <simtoolsgui.h>

@interface SwarmEnvironment: Object <SwarmEnvironment>
{
  id <Arguments> arguments;
  BOOL forceBatchMode;
}
+ createBegin;
- setArguments: (id <Arguments>)arguments;
- setBatchMode: (BOOL)batchMode;
- createEnd;

#import "SwarmEnvironment_getters.h"
@end
