/* Swarm library. Copyright © 2001 Swarm Development Group.
   
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 USA
 
 The Swarm Development Group can be reached via our website at:
 http://www.swarm.org/ */

#ifdef METHOD_FUNCTIONS
struct ActivationOrder_c;
struct ScheduleIndex_c;
struct GroupIndex_c;

extern void _i_ActivationOrder_c__addLast_ (struct ActivationOrder_c *, struct objc_selector *, struct objc_object *);
extern id _i_ScheduleIndex_c__remove (struct ScheduleIndex_c *, struct objc_selector *);
extern id _i_ScheduleIndex_c__nextAction_ (struct ScheduleIndex_c *, struct objc_selector *, id *);
extern id _i_GroupIndex_c__nextAction_ (struct GroupIndex_c *, struct objc_selector *, id *);
extern id _i_ListIndex_mlinks__get (struct ListIndex_mlinks *, struct objc_selector *);
extern id _i_ScheduleIndex_c__get (struct ScheduleIndex_c *, struct objc_selector *);

#define GENERIC_ADD_LAST(l, o) (getClass (l) == id_ActivationOrder_c ? _i_ActivationOrder_c__addLast_ ((struct ActivationOrder_c *) l, M(addLast:), o) : [l addLast: o])

#define SCHEDULE_INDEX_REMOVE(index) _i_ScheduleIndex_c__remove (index, M(remove))

#define GENERIC_NEXTACTION(index, ptr) (getClass (index) == id_ScheduleIndex_c ? _i_ScheduleIndex_c__nextAction_ (index, M(nextAction:), ptr) : getClass (index) == id_GroupIndex_c ? _i_GroupIndex_c__nextAction_ (index, M(nextAction:), ptr) : [index nextAction: ptr])

#define GENERIC_GET(index) (getClass (index) == id_ScheduleIndex_c ? _i_ScheduleIndex_c__get (index, M(get)) : getClass (index) == id_GroupIndex_c ? _i_ListIndex_mlinks__get (index, M(get)) : [index get])
#else

#define GENERIC_ADD_LAST(l, o) [l addLast: o]
#define SCHEDULE_INDEX_REMOVE(index) [index remove]
#define GENERIC_NEXTACTION(index, ptr) [index nextAction: ptr]
#define GENERIC_GET(index) [index get]

#endif

