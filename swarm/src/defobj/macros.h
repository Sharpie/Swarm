#if 1
struct Zone_c;
struct Object_s;
struct ComponentZone_c;

extern id (*_swarm_i_Zone_c__allocIVarsComponent_) (struct Zone_c *, SEL, Class);
extern void (*_swarm_i_Zone_c__freeIVarsComponent_) (struct Zone_c *, SEL, id);
extern void * (*_swarm_i_Zone_c__allocBlock_) (struct Zone_c *, SEL, size_t);
extern void (*_swarm_i_Zone_c__freeBlock_blockSize_) (struct Zone_c *, SEL, void *, size_t);
extern id (*_swarm_i_ComponentZone_c__allocIVars_) (struct ComponentZone_c *, SEL, Class);
extern id (*_swarm_i_Object_s__drop) (struct Object_s *, SEL);

#define ALLOCIVARSCOMPONENT(zone, class) _swarm_i_Zone_c__allocIVarsComponent_ (zone, M(allocIVarsComponent:), class)
#define FREEIVARSCOMPONENT(zone, obj) _swarm_i_Zone_c__freeIVarsComponent_ ((struct Zone_c *) zone, M(freeIVarsComponent:), obj)
#define COMPONENT_ALLOCIVARS(zone,class) _swarm_i_ComponentZone_c__allocIVars_ ((struct ComponentZone_c *) getCZone (zone), M(allocIVars:), class)
#define ALLOCBLOCK(zone, size) _swarm_i_Zone_c__allocBlock_ ((struct Zone_c *) zone, M(allocBlock:), size)
#define FREEBLOCK_SIZE(zone, ptr, size) _swarm_i_Zone_c__freeBlock_blockSize_ ((struct Zone_c *) zone, M(freeBlock:blockSize:), ptr, size);
#define DROP(obj) _swarm_i_Object_s__drop (obj, M(drop)) 

#else
#define ALLOCIVARSCOMPONENT(zone, class) [zone allocIVarsComponent: class]
#define FREEIVARSCOMPONENT(zone, obj) [zone freeIVarsComponent: obj]
#define COMPONENT_ALLOCIVARS(zone,class) [getCZone (zone) allocIVars: class]
#define ALLOCBLOCK(zone, size) [zone allocBlock: size]
#define FREEBLOCK_SIZE(zone, ptr, size) [zone freeBlock: ptr blockSize: size]
#define DROP(obj) [obj drop]
#endif


