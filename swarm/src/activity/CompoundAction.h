// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         CompoundAction.h
Description:  a collection of actions to be performed in a defined order
Library:      activity
*/

#import <collections/Collection.h>

externvar id _activity_activityRefsType;
externvar id _activity_activitySetRefsType;

extern void setDefaultOrder (unsigned *bits, id <Symbol> aSymbol);
extern id <Symbol> getDefaultOrder (unsigned bits);

@interface ActionType_c: Object_s
@end

@interface CompoundAction_c: Collection_any
                         // mixin inheritance for ActionGroup or Schedule
{                        // (variables here are for documentation only)
@public
  id   activityRefs;   // activities currently running this plan
}
#define BitConcurrent       1 << 8   // actions may be executed concurrently
#define BitRandomized       1 << 9   // actions to be executed in random seq
#define BitAutoDrop         1 << 10  // drop actions after execution completed
#define BitRelativeTime     1 << 12  // schedule times are relative to start
#define BitRelTimeSet       1 << 13  // RelativeTime setting not just default
#define BitConcrntGroupSet  1 << 14  // create conc group for single actions
#define BitSingletonGroups  1 << 15  // create conc group for single actions
#define BitConcurrentGroup  1 << 16  // plan is conc group created by schedule
@end
