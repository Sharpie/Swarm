#include <objc/objc.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void *COMclass;
typedef void *COMobject;

extern COMobject swarm_directory_objc_find_object_COM (id object);
extern COMclass swarm_directory_objc_find_COM_class (Class clazz);

extern COMobject swarm_directory_objc_ensure_COM (id object);

extern id swarm_directory_COM_ensure_objc (COMobject cobj);
extern SEL swarm_directory_COM_ensure_selector (COMobject csel);
extern Class swarm_directory_COM_ensure_class (COMclass clazz);
extern COMobject swarm_directory_COM_add_COM (COMobject cObj, id oObj);
#ifdef __cplusplus
}
#endif

#define SD_COM_FIND_CLASS_COM(objcClass) swarm_directory_objc_find_COM_class (objcClass)

#define SD_COM_FIND_OBJECT_COM(objc) swarm_directory_objc_find_object_COM (objc)
#define SD_COM_ENSURE_OBJECT_COM(objc) swarm_directory_objc_ensure_COM (objc)

#define SD_COM_ENSURE_OBJECT_OBJC(cobj) swarm_directory_COM_ensure_objc (cobj)
#define SD_COM_ENSURE_SELECTOR_OBJC(csel) swarm_directory_COM_ensure_selector (csel)
#define SD_COM_ENSURE_CLASS_OBJC(cclazz) swarm_directory_COM_ensure_class (cclazz)
#define SD_COM_ADD_OBJECT_COM(cobj, objc) swarm_directory_COM_add_COM (cobj, objc)


#if 0
#define COM_ENTRY(theObject,theCOMObject) swarm_directory_COM_entry (theCOMObject, theObject)
#define COM_OBJCENTRY(theObject) COM_ENTRY(theObject,0)
#define COM_COMENTRY(theCOMObject) COM_ENTRY(0,theCOMObject)
#define COM_FINDENTRY(theCOMObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->foreignObject.COM = theCOMObject; _findEntry; })
#endif


#if 0
extern id swarm_directory_COM_find_objc (COMobject cobj);
extern fcall_type_t fcall_type_for_COM_class (COMclass clazz);
extern void map_COM_ivars (COMobject cobj,
                           void (*process_object) (const char *name,
                                                   fcall_type_t type,
                                                   void *ptr,
                                                   unsigned rank,
                                                   unsigned *dims));
extern fcall_type_t COM_object_ivar_type (COMobject cobj, const char *ivarName, BOOL *isArrayPtr);
extern unsigned COM_object_getVariableElementCount (COMobject cobj,
                                                    const char *ivarName,
                                                    fcall_type_t itype,
                                                    unsigned irank,
                                                    unsigned *idims);

extern BOOL COM_selector_p (COMobject csel);
extern const char *COM_ensure_selector_type_signature (COMobject csel);
extern const char *COM_get_class_name (COMclass clazz);
extern void COM_object_setVariable (COMobject obj, const char *ivarName, void *inbuf);

extern void swarm_directory_COM_associate_objects (COMobject swarmEnvironment);

extern DirectoryEntry *swarm_directory_COM_add_selector (SEL sel, COMobject cobj);

extern void swarm_directory_COM_switch_phase (id nextPhase, COMobject currentPhase);
extern void swarm_directory_COM_switch_objc (id object, COMobject cobj);
extern COMobject swarm_directory_COM_next_phase (COMobject cobj);
#define SD_COM_FIND_SELECTOR_COM(objc) swarm_directory_objc_find_selector_COM (objc)
extern COMobject swarm_directory_objc_find_selector_COM (SEL sel);
extern Class swarm_directory_COM_find_class_named (const char *className);
#define SD_COM_FINDOBJC(cobj)  swarm_directory_COM_find_objc (cobj)
#define SD_COM_SWITCHPHASE(cobj, objc) swarm_directory_COM_switch_phase (objc, cobj)
#define SD_COM_SWITCHOBJC(cobj, newobjc) swarm_directory_COM_switch_objc (newobjc, cobj)
#define SD_COM_NEXTPHASE(cobj) swarm_directory_COM_next_phase (cobj)

extern const char *COM_class_name (COMobject cobj);
extern void COM_drop (COMobject cobj);

extern Class swarm_directory_COM_class_for_object (COMobject cobj);
#endif
