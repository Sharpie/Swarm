#ifdef METHOD_FUNCTIONS
struct List_linked;
struct ListIndex_mlinks;
struct List_mlinks;
struct MapIndex_c;
struct Map_c;
struct OrderedSet_c;

extern id _i_Collection_any_remove_ (id, SEL, id);
extern id _i_Collection_any_atOffset_ (id, SEL, unsigned);
extern id _i_Collection_any_getFirst (id, SEL);
extern id _i_Collection_any_getLast (id, SEL);

#define COLLECTION_REMOVE(coll, obj) _i_Collection_any_remove_(coll, M(remove:), obj)
#define COLLECTION_ATOFFSET(coll, offset) _i_Collection_any_atOffset_ (coll, M(atAOffset:), offset)
#define COLLECTION_GETLAST(coll) _i_Collection_any_getLast (coll, M(getLast))
#define COLLECTION_GETFIRST(coll) _i_Collection_any_getFirst (coll, M(getFirst))
extern id _i_List_linked__removeLast (id, SEL);
#define LIST_REMOVELAST(list) _i_List_linked__removeLast(list, M(removeLast))
extern id _i_List_linked__removeFirst (id, SEL);
#define LIST_REMOVEFIRST(list) _i_List_linked__removeFirst(list, M(removeFirst))
extern id _i_List_linked__addLast_ (id, SEL, id);
#define LIST_ADDLAST(list, obj) _i_List_linked__addLast_(list, M(addLast), obj)
extern id _i_List_linked__addFirst_ (id, SEL, id);
#define LIST_ADDFIRST(list, obj) _i_List_linked__addFirst_(list, M(addFirst), obj)

extern id _i_ListIndex_linked__next (id, SEL);
extern id _i_ListIndex_linked__prev (id, SEL);
extern id <Symbol> _i_ListIndex_linked__getLoc (id, SEL);
extern id _i_ListIndex_linked__setLoc_ (id, SEL, id);
extern id _i_ListIndex_linked__get (id, SEL);
extern id _i_ListIndex_linked__remove (id, SEL);
extern id _i_ListIndex_linked__addBefore_ (id, SEL, id);

extern void _i_ListIndex_mlinks__setLoc_ (id, SEL, id);
extern id _i_List_mlinks__begin_ (id, SEL, id);

extern id _i_ListIndex_mlinks__remove (struct ListIndex_mlinks *, struct objc_selector *);
extern id _i_List_mlinks__createIndex_fromMember_ (struct List_mlinks *, struct objc_selector *, struct objc_object *, struct objc_object *);
extern void _i_List_mlinks__addLast_ (struct List_mlinks *, struct objc_selector *, struct objc_object *);

extern id _i_Map_c__createIndex_fromMember_ (struct Map_c *, struct objc_selector *, id, id);
extern BOOL _i_Map_c__at_memberSlot_ (struct Map_c *, struct objc_selector *, id, id **);
extern id _i_MapIndex_c__next (struct MapIndex_c *, struct objc_selector *);
extern id _i_MapIndex_c__next_ (struct MapIndex_c *, struct objc_selector *, id *);
extern id _i_MapIndex_c__prev (struct MapIndex_c *, struct objc_selector *);
extern void _i_MapIndex_c__setLoc_ (struct MapIndex_c *, struct objc_selector *, id <Symbol>);
extern id <Symbol> _i_MapIndex_c__getLoc (struct MapIndex_c *, struct objc_selector *);
extern BOOL _i_Map_c__at_insert_ (struct Map_c *, struct objc_selector *, id, id);
extern id _i_OrderedSet_c__remove_ (struct OrderedSet_c *, struct objc_selector *, id);

#define LIST_BEGIN(list) beginLinkedList (list)

#define LIST_INDEX_NEXT(index) _i_ListIndex_linked__next (index, M(next))
#define LIST_INDEX_PREV(index) _i_ListIndex_linked__prev (index, M(prev))
#define LIST_INDEX_GETLOC(index) _i_ListIndex_linked__getLoc (index, M(getLoc))
#define LIST_INDEX_SETLOC(index, sym) _i_ListIndex_linked__setLoc_ (index, M(setLoc), sym)
#define LIST_INDEX_GET(index) _i_ListIndex_linked__get (index, M(get))
#define LIST_INDEX_REMOVE(index) _i_ListIndex_linked__remove (index, M(remove))
#define LIST_INDEX_ADDBEFORE(index, obj) _i_ListIndex_linked__addBefore_ (index, M(addBefore:), obj)

#define MLINK_INDEX_REMOVE(index) _i_ListIndex_mlinks__remove ((struct ListIndex_mlinks *) index, M(remove))
#define MLINK_CREATEINDEX_FROMMEMBER(list, zone, obj) _i_List_mlinks__createIndex_fromMember_ ((struct List_mlinks *) list, M(createIndex:fromMember:), zone, obj)
#define MLINK_ADD(list, obj) _i_List_mlinks__addLast_ ((struct List_mlinks *) list, M(addLast:), obj)
#define MLINK_INDEX_SETLOC(index, sym) _i_ListIndex_mlinks__setLoc_ (index, M(setLoc:), sym)
#define MLINK_BEGIN(index, zone) _i_List_mlinks__begin_ (index, M(begin:), zone)

#define MAP_CREATEINDEX_FROMMEMBER(map, zone, obj) _i_Map_c__createIndex_fromMember_ (map, M(createIndex:fromMember:), zone, obj)
#define MAP_AT_MEMBERSLOT(map, key, ptr) _i_Map_c__at_memberSlot_ (map, M(at:memberSlot:), key, ptr)
#define MAP_AT_INSERT(map, key, value) _i_Map_c__at_insert_ (self, M(at:insert:), key, value)
#define MAP_INDEX_NEXT(index) _i_MapIndex_c__next (index, M(next))
#define MAP_INDEX_NEXTKEY(mapindex, keyptr) _i_MapIndex_c__next_ (mapindex, M(next:), keyptr)
#define MAP_INDEX_PREV(index) _i_MapIndex_c__prev (index, M(prev))
#define MAP_INDEX_SETLOC(index, sym) _i_MapIndex_c__setLoc_ (index, M(setLoc:), sym)
#define MAP_INDEX_GETLOC(index) _i_MapIndex_c__getLoc (index, M(getLoc:))

#define ORDEREDSET_REMOVE(set, obj) _i_OrderedSet_c__remove_ (set, M(remove:), obj)

#else

#define COLLECTION_REMOVE(coll, obj) [coll remove: obj]
#define COLLECTION_ATOFFSET(coll, offset) [coll atOffset: offset]
#define COLLECTION_GETLAST(coll) [coll getLast]
#define COLLECTION_GETFIRST(coll) [coll getFirst]

#define LIST_REMOVELAST(list) [list removeLast]
#define LIST_REMOVEFIRST(list) [list removeFirst]
#define LIST_ADDFIRST(list, obj) [list addFirst: obj]
#define LIST_ADDLAST(list, obj) [list addLast: obj]

#define LIST_BEGIN(list) beginLinkedList (list)
#define LIST_INDEX_NEXT(index) [index next]
#define LIST_INDEX_PREV(index) [index prev]
#define LIST_INDEX_GETLOC(index) [index getLoc]
#define LIST_INDEX_SETLOC(index, sym) [index setLoc: sym]
#define LIST_INDEX_GET(index) [index get]
#define LIST_INDEX_REMOVE(index) [index remove]
#define LIST_INDEX_ADDBEFORE(index, obj) [index addBefore: obj]

#define MLINK_INDEX_REMOVE(index) [index remove]
#define MLINK_CREATEINDEX_FROMMEMBER(list, zone, obj) [list createIndex: zone fromMember: obj]
#define MLINK_ADD(list, obj) [list addLast: obj]
#define MLINK_INDEX_SETLOC(index, sym) [index setLoc: sym]
#define MLINK_BEGIN(list, zone) [list begin: zone]

#define MAP_CREATEINDEX_FROMMEMBER(map, zone, obj) [map createIndex: zone fromMember: obj]
#define MAP_AT_MEMBERSLOT(map, key, ptr) [map at: key memberSlot: ptr]
#define MAP_AT_INSERT(map, key, value) [map at: key insert: value]
#define MAP_INDEX_NEXT(index) [index next]
#define MAP_INDEX_NEXTKEY(index, keyptr) [index next: keyptr]
#define MAP_INDEX_PREV(index) [index prev]
#define MAP_INDEX_SETLOC(index, sym) [index setLoc: sym]
#define MAP_INDEX_GETLOC(index) [index getLoc]

#define ORDEREDSET_REMOVE(set, obj) [set remove: obj]

#endif

#define COLLECTION_COUNT(coll) (((Collection_any *) coll)->count)
#define LIST_GETFIRST(l) ((((List_linked *) l)->firstLink)->refObject)

