#ifdef METHOD_FUNCTIONS
struct Zone_c;
struct Object_s;
struct ComponentZone_c;

extern id _i_Zone_c__allocIVarsComponent_ (struct Zone_c *, struct objc_selector *, Class);
extern void _i_Zone_c__freeIVarsComponent_ (struct Zone_c *, struct objc_selector *, id);
extern void *_i_Zone_c__allocBlock_ (struct Zone_c *, struct objc_selector *, size_t);
extern void _i_Zone_c__freeBlock_blockSize_ (struct Zone_c *, struct objc_selector *, void *, size_t);
extern id _i_ComponentZone_c__allocIVars_ (struct ComponentZone_c *, struct objc_selector *, Class);

#define ALLOCIVARSCOMPONENT(zone, class) _i_Zone_c__allocIVarsComponent_ (zone, M(allocIVarsComponent:), class)
#define FREEIVARSCOMPONENT(zone, obj) _i_Zone_c__freeIVarsComponent_ ((struct Zone_c *) zone, M(freeIVarsComponent:), obj)
#define COMPONENT_ALLOCIVARS(zone,class) _i_ComponentZone_c__allocIVars_ ((struct ComponentZone_c *) getCZone (zone), M(allocIVars:), class)
#define ALLOCBLOCK(zone, size) _i_Zone_c__allocBlock_ ((struct Zone_c *) zone, M(allocBlock:), size)
#define FREEBLOCK_SIZE(zone, ptr, size) _i_Zone_c__freeBlock_blockSize_ ((struct Zone_c *) zone, M(freeBlock:blockSize:), ptr, size);
#define DROP(obj) _i_Object_s__drop (obj, M(drop)) 


#else
#define ALLOCIVARSCOMPONENT(zone, class) [zone allocIVarsComponent: class]
#define FREEIVARSCOMPONENT(zone, obj) [zone freeIVarsComponent: obj]
#define COMPONENT_ALLOCIVARS(zone,class) [getCZone (zone) allocIVars: class]
#define ALLOCBLOCK(zone, size) [zone allocBlock: size]
#define FREEBLOCK_SIZE(zone, ptr, size) [zone freeBlock: ptr blockSize: size]
#define DROP(obj) [obj drop]
#endif


