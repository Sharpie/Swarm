#ifndef __defobj_COM_h
#define __defobj_COM_h

#include <defobj.h>
#include <objc/objc.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const void *COMclass;
typedef const void *COMobject;
typedef const void *COMselector;

struct COMInterface;

typedef const struct COMInterface COMEnv;

struct COMInterface {
  void *(*createComponent) (COMclass cClass);
  void *(*findComponent) (const char *componentName);
  const char *(*copyString) (const char *str);
  const char *(*getName) (COMobject cObj);
  
  COMobject (*normalize) (COMobject);

  COMselector (*selectorQuery) (COMobject cObj);
  BOOL (*selectorIsVoidReturn) (COMselector cSel);
  BOOL (*selectorIsBooleanReturn) (COMselector cSel);
  const char *(*selectorName) (COMselector cSel);
  unsigned (*selectorArgCount) (COMselector cSel);
  fcall_type_t (*selectorArgFcallType) (COMselector cSel, unsigned index);
  void (*selectorInvoke) (COMselector cSel, void *args);

  void *(*createArgVector) (unsigned size);
  void (*setArg) (void *args, unsigned pos, fcall_type_t type, types_t *value);
  void (*setReturn) (void *args, unsigned pos, fcall_type_t type, void *value);
  void (*freeArgVector) (void *args);
};

extern void initCOM (COMEnv *env);
extern BOOL COM_init_p ();
extern COMobject swarm_directory_objc_find_object_COM (id oObject);
extern id swarm_directory_COM_find_object_objc (COMobject cobj);

extern COMclass swarm_directory_objc_find_COM_class (Class oClass);

extern COMobject swarm_directory_objc_ensure_object_COM (id object);

extern id swarm_directory_COM_ensure_object_objc (COMobject cObject);
extern SEL swarm_directory_COM_ensure_selector (COMselector cSelector);
extern Class swarm_directory_COM_ensure_class (COMclass cClass);
extern COMobject swarm_directory_COM_add_object_COM (COMobject cObject, id oObject);
extern id swarm_directory_COM_add_object_objc (COMobject cObject, id oObject);
extern const char *COM_copy_string (const char *str);
extern const char *COM_class_name (COMobject cObj);

extern BOOL COM_selector_is_boolean_return (COMselector cSel);
extern void COM_selector_invoke (COMselector cSel, void *args);
extern void *COM_create_arg_vector (unsigned size);
extern void COM_set_arg (void *args, unsigned pos, fcall_type_t type, types_t *value);
extern void COM_set_return (void *args, unsigned pos, fcall_type_t type, void *value);
extern void COM_free_arg_vector (void *args);

extern COMobject swarm_directory_objc_find_selector_COM (SEL oSel);
extern void swarm_directory_COM_add_selector (COMselector cSel, SEL oSel);
extern COMobject swarm_directory_update_phase_COM (id oObj);

#ifdef __cplusplus
}
#endif

#define SD_COM_FIND_CLASS_COM(oClass) swarm_directory_objc_find_COM_class (oClass)
#define SD_COM_FIND_CLASS_COM_RETURN(type,oClass) *ret = NS_STATIC_CAST(type, SD_COM_FIND_CLASS_COM (oClass))

#define SD_COM_FIND_OBJECT_COM(oObject) swarm_directory_objc_find_object_COM (oObject)
#define SD_COM_FIND_OBJECT_OBJC(cObject) swarm_directory_COM_find_object_objc (cObject)

#define SD_COM_FIND_SELECTOR_COM(sel) swarm_directory_objc_find_selector_COM (sel)
#define SD_COM_FIND_SELECTOR_COM_RETURN(type,oSelector) *ret = NS_STATIC_CAST(type, SD_COM_FIND_SELECTOR_COM (oSelector))

#define SD_COM_ENSURE_OBJECT_COM(oObject) swarm_directory_objc_ensure_object_COM (oObject)

#define SD_COM_ENSURE_OBJECT_OBJC(cObject) swarm_directory_COM_ensure_object_objc (cObject)

#define SD_COM_ENSURE_SELECTOR_OBJC(cSelector) swarm_directory_COM_ensure_selector (cSelector)

#define SD_COM_ENSURE_CLASS_OBJC(cClass) swarm_directory_COM_ensure_class (cClass)

#define SD_COM_ADD_OBJECT_COM(cObject, oObject) swarm_directory_COM_add_object_COM (cObject, oObject)

#define SD_COM_ADD_OBJECT_OBJC(cObject, oObject) swarm_directory_COM_add_object_objc (cObject, oObject)

#define SD_COM_ADD_CLASS_COM(cClass, oClass) swarm_directory_COM_add_object_COM (cClass, (id) cClass)
#define SD_COM_ADD_SELECTOR(cSel, oSel) swarm_directory_COM_add_selector (cSel, oSel)

#ifdef __cplusplus
#define SD_COM_ENSURE_OBJECT_COM_RETURN(oObject,type) (NS_STATIC_CAST (nsISupports *, SD_COM_ENSURE_OBJECT_COM (oObject))->QueryInterface (NS_GET_IID (type), (void **) ret))
#define SD_COM_ENSURE_THIS_OBJECT_OBJC() SD_COM_ENSURE_OBJECT_OBJC(NS_STATIC_CAST(swarmITyping*,this))
#define SD_COM_ADD_THIS_OBJECT_COM(oObject) SD_COM_ADD_OBJECT_COM (NS_STATIC_CAST(swarmITyping*,this),oObject)
#define SD_COM_UPDATE_PHASE_RETURN(oObject, type) *ret = NS_STATIC_CAST (type, swarm_directory_update_phase_COM (oObject))
#endif

#define SD_COM_COPY_STRING(str) COM_copy_string (str)

#define COM_FIND_OBJECT_ENTRY(theCOMObject) ({ ObjectEntry *_findEntry  = alloca (sizeof (ObjectEntry)); _findEntry->foreignObject.COM = (COMOBJECT) theCOMObject; _findEntry; })
#define COM_OBJECT_ENTRY(theCOMObject, theObject) [[[[ObjectEntry createBegin: globalZone] setCOMObject: (COMOBJECT) theCOMObject] setObject: theObject] createEnd]
#define COM_SELECTOR_ENTRY(theCOMObject, theSelector) [[[[SelectorEntry createBegin: globalZone] setCOMObject: (COMOBJECT) theCOMObject] setSelector: theSelector] createEnd]



#if 0
#define COM_ENTRY(theObject,theCOMObject) swarm_directory_COM_entry (theCOMObject, theObject)
#define COM_OBJCENTRY(theObject) COM_ENTRY(theObject,0)
#define COM_COMENTRY(theCOMObject) COM_ENTRY(0,theCOMObject)
#define COM_FINDENTRY(theCOMObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->foreignObject.COM = theCOMObject; _findEntry; })
#endif


#if 0
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

extern BOOL COM_selector_p (COMselector csel);
extern const char *COM_ensure_selector_type_signature (COMselector csel);
extern const char *COM_get_class_name (COMclass clazz);
extern void COM_object_setVariable (COMobject obj, const char *ivarName, void *inbuf);

extern void swarm_directory_COM_associate_objects (COMobject swarmEnvironment);

extern void swarm_directory_COM_switch_phase (id nextPhase, COMobject currentPhase);
extern void swarm_directory_COM_switch_objc (id object, COMobject cobj);
extern COMobject swarm_directory_COM_next_phase (COMobject cobj);
extern Class swarm_directory_COM_find_class_named (const char *className);
#define SD_COM_FINDOBJC(cobj)  swarm_directory_COM_find_objc (cobj)
#define SD_COM_SWITCHPHASE(cobj, objc) swarm_directory_COM_switch_phase (objc, cobj)
#define SD_COM_SWITCHOBJC(cobj, newobjc) swarm_directory_COM_switch_objc (newobjc, cobj)
#define SD_COM_NEXTPHASE(cobj) swarm_directory_COM_next_phase (cobj)

extern void COM_drop (COMobject cobj);

extern Class swarm_directory_COM_class_for_object (COMobject cobj);
#endif
#endif
