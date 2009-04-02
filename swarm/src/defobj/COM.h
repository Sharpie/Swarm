#ifndef __defobj_COM_h
#define __defobj_COM_h

#include <Swarm/defobj.h>
#import <Swarm/swarm-objc-api.h>

#if defined(__cplusplus) && !defined(__OBJC__)
extern "C" {
#endif

typedef void *COMclass;
typedef void *COMobject;
typedef void *COMselector;
typedef void *COMmethod;

struct COMInterface;

typedef const struct COMInterface COMEnv;
typedef void (*COM_collect_variable_func_t) (COMmethod getterMethod, COMmethod setterMethod);
typedef void (*COM_collect_method_func_t) (COMmethod method);
typedef void (*JS_collect_func_t) (const char *name);

struct COMInterface {
  COMclass (*COMcopyComponentID) (COMclass cClass);
  COMclass (*COMgetClass) (COMobject cObj);
  COMclass (*COMfindComponent) (const char *componentName);
  const char *(*COMgetComponentName) (COMclass cClass);
  
  COMobject (*COMcreateComponent) (COMclass cClass);

  const char *(*COMgetName) (COMobject cObj);

  COMobject (*COMnormalize) (COMobject);

  const char *(*COMcopyString) (const char *str);

  COMselector (*selectorCreate) (COMmethod cMethod);
  COMmethod (*selectorMethod) (COMselector cSel);
  COMselector (*selectorQuery) (COMobject cObj);
  BOOL (*selectorIsJavaScript) (COMselector cSel);
  BOOL (*selectorIsVoidReturn) (COMselector cSel);
  BOOL (*selectorIsBooleanReturn) (COMselector cSel);
  const char *(*selectorName) (COMselector cSel);
  unsigned (*selectorArgCount) (COMselector cSel);
  fcall_type_t (*selectorArgFcallType) (COMselector cSel, unsigned index);

  void *(*COMcreateParams) (unsigned size);
  void (*COMsetArg) (void *params, unsigned pos, val_t *val);
  void (*COMsetReturn) (void *params, unsigned pos, val_t *val);
  void (*COMfreeParams) (void *params);

  void (*collectMethods) (COMclass cClass, COM_collect_variable_func_t variableFunc, COM_collect_method_func_t methodFunc);

  const char *(*COMmethodName) (COMmethod cMethod);
  unsigned (*COMmethodArgCount) (COMmethod cMethod);
  fcall_type_t (*COMmethodParamFcallType) (COMmethod cMethod, unsigned index);
  void (*COMmethodSetReturn) (COMmethod cMethod, void *params, void *value);
  void (*COMmethodInvoke) (COMobject target, COMmethod cMethod, void *params);

  BOOL (*isJavaScript) (COMobject cObj);
  void *(*JScreateParams) (unsigned size);
  void (*JSsetArg) (void *params, unsigned pos, val_t *val);
  void (*JSsetReturn) (void *params, unsigned pos, val_t *val);
  void (*JSfreeParams) (void *params);
  
  BOOL (*JSprobeVariable) (COMobject cObj, const char *variableName, val_t *ret);
  void (*JSsetVariable) (COMobject cObj, const char *variableName, val_t *ret);
  void (*JSmethodInvoke) (COMobject cObj, const char *methodName, void *params);
  unsigned (*JSmethodArgCount) (COMobject cObj, const char *methodName);
  void (*collectJSProperties) (COMobject cObj, JS_collect_func_t variableFunc, JS_collect_func_t methodFunc);

};

extern void initCOM (COMEnv *env);
extern BOOL COM_init_p ();
extern COMobject swarm_directory_objc_find_object_COM (id oObject);
extern id swarm_directory_COM_find_object_objc (COMobject cobj);

extern COMclass swarm_directory_objc_find_class_COM (Class oClass);
extern Class swarm_directory_COM_find_class_objc (COMclass cClass);

extern COMobject swarm_directory_objc_ensure_object_COM (id object);

extern id swarm_directory_COM_ensure_object_objc (COMobject cObject);
extern SEL swarm_directory_COM_ensure_selector (COMselector cSelector);
extern Class swarm_directory_COM_ensure_class_objc (COMclass cClass);
extern COMobject swarm_directory_COM_add_object_COM (COMobject cObject, id oObject);
extern COMclass swarm_directory_COM_add_class_COM (COMclass cClass, Class oClass);
extern id swarm_directory_COM_add_object_objc (COMobject cObject, id oObject);
extern const char *COM_copy_string (const char *str);
extern const char *COM_class_name (COMobject cObj);
extern const char *COM_get_class_name (COMclass cClass);
extern COMclass COM_get_class (COMobject cObj);
extern COMclass COM_find_class (const char *className);

extern BOOL COM_selector_is_javascript (COMselector cSel);
extern BOOL COM_selector_is_boolean_return (COMselector cSel);
extern COMselector COM_selector_create (COMmethod cMethod);
extern COMmethod COM_selector_method (COMselector cSel);

extern void *COM_create_params (unsigned size);
extern void COM_set_arg (void *params, unsigned pos, val_t *val);
extern void COM_set_return (void *params, unsigned pos, val_t *val);
extern void COM_free_params (void *params);

extern BOOL COM_is_javascript (COMobject cObj);

extern void *JS_create_params (unsigned size);
extern void JS_set_arg (void *params, unsigned pos, val_t *val);
extern void JS_set_return (void *params, unsigned pos, val_t *val);
extern void JS_free_params (void *params);

extern BOOL JS_probe_variable (COMobject cObj, const char *variableName, val_t *ret);
extern void JS_set_variable (COMobject cObj, const char *variableName, val_t *val);
extern void JS_method_invoke (COMobject cObj, const char *methodName, void *params);
extern unsigned JS_method_arg_count (COMobject cObj, const char *methodName);


extern COMobject swarm_directory_objc_find_selector_COM (SEL oSel);
extern void swarm_directory_COM_add_selector (COMselector cSel, SEL oSel);
extern COMobject swarm_directory_update_phase_COM (id obj);

extern void COM_collect_variables (COMclass cClass, COM_collect_variable_func_t variableFunc);
extern void COM_collect_methods (COMclass cClass, COM_collect_method_func_t methodFunc);
extern void JS_collect_variables (COMobject cObj, JS_collect_func_t variableFunc);
extern void JS_collect_methods (COMobject cObj, JS_collect_func_t methodsFunc);

extern const char *COM_method_name (COMmethod cMethod);
extern void COM_method_set_return (COMmethod cMethod, void *params, void *value);
extern void COM_method_invoke (COMobject target, COMmethod cMethod, void *params);
extern fcall_type_t COM_method_param_fcall_type (COMmethod cMethod, unsigned index);
#ifdef __cplusplus
}
#endif

#define SD_COM_FIND_CLASS_COM(oClass) swarm_directory_objc_find_class_COM (oClass)
#define SD_COM_FIND_CLASS_COM_RETURN(type,oClass) *ret = NS_STATIC_CAST(type, SD_COM_FIND_CLASS_COM (oClass))
#define SD_COM_FIND_CLASS_OBJC(cClass) swarm_directory_COM_find_class_objc (cClass)

#define SD_COM_FIND_OBJECT_COM(oObject) swarm_directory_objc_find_object_COM (oObject)
#define SD_COM_FIND_OBJECT_OBJC(cObject) swarm_directory_COM_find_object_objc (cObject)

#define SD_COM_FIND_SELECTOR_COM(sel) swarm_directory_objc_find_selector_COM (sel)
#define SD_COM_FIND_SELECTOR_COM_RETURN(type,oSelector) *ret = NS_STATIC_CAST(type, SD_COM_FIND_SELECTOR_COM (oSelector))

#define SD_COM_ENSURE_OBJECT_OBJC(cObject) swarm_directory_COM_ensure_object_objc (cObject)

#define SD_COM_ENSURE_SELECTOR_OBJC(cSelector) swarm_directory_COM_ensure_selector (cSelector)

#define SD_COM_ENSURE_CLASS_OBJC(cClass) swarm_directory_COM_ensure_class_objc ((COMclass) cClass)

#define SD_COM_ADD_OBJECT_OBJC(cObject, oObject) swarm_directory_COM_add_object_objc (cObject, oObject)

#define SD_COM_ADD_CLASS_COM(cClass, oClass) swarm_directory_COM_add_class_COM (cClass, oClass)
#define SD_COM_ADD_SELECTOR(cSel, oSel) swarm_directory_COM_add_selector (cSel, oSel)

#define SD_COM_COPY_STRING(str) COM_copy_string (str)

#define COM_FIND_OBJECT_ENTRY(theCOMObject) ({ ObjectEntry *_findEntry  = alloca (sizeof (ObjectEntry)); _findEntry->foreignObject.COM = (COMOBJECT) theCOMObject; _findEntry; })
#define COM_OBJECT_ENTRY(theCOMObject, theObject) [[[[ObjectEntry createBegin: globalZone] setCOMObject: (COMOBJECT) theCOMObject] setObject: theObject] createEnd]
#define COM_SELECTOR_ENTRY(theCOMObject, theSelector) [[(id <FArguments>)[[SelectorEntry createBegin: globalZone] setCOMObject: (COMOBJECT) theCOMObject] setSelector: theSelector] createEnd]



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
