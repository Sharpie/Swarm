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

