#import <defobj/deftype.h>
#import <activity.h>

@protocol Group_test
CREATING
+ createBegin: aZone numberOfObjects: (int)num;
- addObject: obj;
USING
- (void)describe: outputCharStream;
- (void)describeForEach: outputCharStream;
- getObjectAt: (int)offset; 
@end

@protocol ActionGroup_test <Group_test, ActionGroup, CREATABLE>
@end

@protocol ConcurrentGroup_test <Group_test, ConcurrentGroup, CREATABLE>
@end

#import <tactivity/types.h>
