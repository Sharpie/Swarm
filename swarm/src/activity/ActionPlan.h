// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         ActionPlan.h
Description:  a collection of actions to be performed in a defined order
Library:      activity
*/

#import <collections/Collection.h>
#import <activity.h>

extern id   _activity_activityRefsType;
extern int  _activity_compareIDs( id, id );

@interface ActionPlan_c : Collection_any
                         // mixin inheritance for ActionGroup or Schedule
{                        // (variables here are for documentation only)
@public
  id   variableDefs;   // array of variable objects defined for plan
  id   activityRefs;   // activities currently running this plan
}
#define Bit_Concurrent       1 << 8   // actions may be executed concurrently
#define Bit_Randomized       1 << 9   // actions to be executed in random seq
#define Bit_AutoDrop         1 << 10  // drop actions after execution completed
#define Bit_RelativeTime     1 << 12  // schedule times are relative to start
#define Bit_RelTimeSet       1 << 13  // RelativeTime setting not just default
#define Bit_ConcrntGroupSet  1 << 14  // create conc group for single actions
#define Bit_SingletonGroups  1 << 15  // create conc group for single actions
#define Bit_ConcurrentGroup  1 << 16  // plan is conc group created by schedule 
#define Bit_ResultVarDef     1 << 17  // result variable defined 
@end

@interface ActionPlan_create : ActionPlan_c
/*** methods implemented in .m file ***/
+ implement: (id *)typeID;
@end

@interface VariableDefinition_c : Object_s
{
@public
  ActionPlan_c  *actionPlan;
  int           valueOffset;
}
- getActionPlan;
@end

@interface ArgumentDefinition_c : VariableDefinition_c
@end

@interface ResultDefinition_c : VariableDefinition_c
@end
