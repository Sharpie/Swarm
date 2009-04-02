// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:         CompoundAction.h
Description:  a collection of actions to be performed in a defined order
Library:      activity
*/

#import <Swarm/Collection.h>

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
