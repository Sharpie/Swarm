#import "COM.h"
#import <defobj/directory.h>
#import <defobj/COMProxy.h>

// mframe_build_signature
#ifdef GNUSTEP
#include <Foundation/NSMethodSignature.h>
#else
#ifdef USE_MFRAME
#include <objc/mframe.h>
#endif
#endif

#import "internal.h" // objc_type_for_fcall_type
#include <misc.h> // strdup

static COMEnv *comEnv = 0;

void
initCOM (COMEnv *env)
{
  comEnv = env;
}

BOOL
COM_init_p ()
{
  return comEnv != 0;
}

const char *
COM_copy_string (const char *str)
{
  return comEnv->COMcopyString (str);
}

const char *
COM_class_name (COMobject cObj)
{
  return SSTRDUP (comEnv->COMgetName (cObj));
}

const char *
COM_get_class_name (COMclass cClass)
{
  return SSTRDUP (comEnv->COMgetComponentName (cClass));
}

COMclass
COM_get_class (COMobject cObj)
{
  if (comEnv)
    return comEnv->COMgetClass (cObj);
  return NULL;
}

COMclass
COM_find_class (const char *className)
{
  if (comEnv)
    return comEnv->COMfindComponent (className);
  return NULL;
}

BOOL
COM_selector_is_javascript (COMselector cSel)
{
  return comEnv->selectorIsJavaScript (cSel);
}

BOOL
COM_selector_is_boolean_return (COMselector cSel)
{
  return comEnv->selectorIsBooleanReturn (cSel);
}

COMselector
COM_selector_create (COMmethod cMethod)
{
  return comEnv->selectorCreate (cMethod);
}

COMmethod
COM_selector_method (COMselector cSel)
{
  return comEnv->selectorMethod (cSel);
}

void *
COM_create_params (unsigned size)
{
  return comEnv->COMcreateParams (size);
}

void
COM_set_arg (void *params, unsigned pos, val_t *val)
{
  comEnv->COMsetArg (params, pos, val);
}

void
COM_set_return (void *params, unsigned pos, val_t *val)
{
  comEnv->COMsetReturn (params, pos, val);
}

void
COM_free_params (void *params)
{
  comEnv->COMfreeParams (params);
}

BOOL
COM_is_javascript (COMobject cObj)
{
  return cObj ? comEnv->isJavaScript (cObj) : NO;
}

void *
JS_create_params (unsigned size)
{
  return comEnv->JScreateParams (size);
}

void
JS_set_arg (void *params, unsigned pos, val_t *val)
{
  comEnv->JSsetArg (params, pos, val);
}

void
JS_set_return (void *params, unsigned pos, val_t *val)
{
  comEnv->JSsetReturn (params, pos, val);
}

void
JS_free_params (void *params)
{
  comEnv->JSfreeParams (params);
}

void
COM_collect_variables (COMclass cClass, COM_collect_variable_func_t variableFunc)
{
  comEnv->collectMethods (cClass, variableFunc, NULL);
}

void
COM_collect_methods (COMclass cClass, COM_collect_method_func_t methodFunc)
{
  comEnv->collectMethods (cClass, NULL, methodFunc);
}

void
JS_collect_variables (COMobject cObj, JS_collect_func_t variableFunc)
{
  comEnv->collectJSProperties (cObj, variableFunc, NULL);
}

void
JS_collect_methods (COMobject cObj, JS_collect_func_t methodFunc)
{
  comEnv->collectJSProperties (cObj, NULL, methodFunc);
}

BOOL
JS_probe_variable (COMobject cObj, const char *variableName, val_t *ret)
{
  return comEnv->JSprobeVariable (cObj, variableName, ret);
}

void
JS_set_variable (COMobject cObj, const char *variableName, val_t *val)
{
  comEnv->JSsetVariable (cObj, variableName, val);
}

void
JS_method_invoke (COMobject cObj, const char *methodName, void *params)
{
  comEnv->JSmethodInvoke (cObj, methodName, params);
}

unsigned
JS_method_arg_count (COMobject cObj, const char *methodName)
{
  return comEnv->JSmethodArgCount (cObj, methodName);
}

const char *
COM_method_name (COMmethod cMethod)
{
  return comEnv->COMmethodName (cMethod);
}

fcall_type_t
COM_method_param_fcall_type (COMmethod cMethod, unsigned index)
{
  return comEnv->COMmethodParamFcallType (cMethod, index);
}

void
COM_method_set_return (COMmethod cMethod, void *params, void *value)
{
  comEnv->COMmethodSetReturn (cMethod, params, value);
}

void
COM_method_invoke (COMobject cObj, COMmethod cMethod, void *params)
{
  comEnv->COMmethodInvoke (cObj, cMethod, params);
}

COMobject 
swarm_directory_objc_find_object_COM (id oObject)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (oObject);
  
  if (entry)
    {
      if (entry->type == foreign_COM)
        return entry->foreignObject.COM;
    }
  return NULL;
}

COMselector
swarm_directory_objc_find_selector_COM (SEL sel)
{
  SelectorEntry *entry = swarm_directory_objc_find_selector (sel);

  if (entry)
    {
      if (entry->type == foreign_COM)
        return comEnv->selectorQuery (entry->foreignObject.COM);
    }
  return NULL;
}

static COMclass
find_wrapper_class_COM (Class oClass)
{
  if (comEnv)
    {
      const char *name = language_independent_class_name_for_objc_class (oClass);
      COMclass cClass = comEnv->COMfindComponent (name);
      
      FREECLASSNAME (name);
      return cClass;
    }
  return NULL;
}

COMclass
swarm_directory_objc_find_class_COM (Class oClass)
{
  if (swarmDirectory) // for find_wrapper_class_COM
    {
      COMclass cClass = (COMclass) SD_COM_FIND_OBJECT_COM ((id) oClass);
      
      if (!cClass)
        {
          cClass = find_wrapper_class_COM (oClass);
          if (cClass)
            cClass = SD_COM_ADD_CLASS_COM (cClass, oClass);
        }
      return cClass;
    }
  else
    return NULL;
}

COMobject
swarm_directory_objc_ensure_object_COM (id oObject)
{
  COMobject cObject;

  if (!oObject)
    return 0;
  
  if (!(cObject = SD_COM_FIND_OBJECT_COM (oObject)))
    {
      Class oClass = getClass (oObject);
      COMclass cClass = SD_COM_FIND_CLASS_COM (oClass);
      
      cObject = comEnv->COMnormalize (comEnv->COMcreateComponent (cClass));
      return swarm_directory_COM_add_object_COM (cObject, oObject);
    }
  return cObject;
}

COMobject
swarm_directory_update_phase_COM (id obj)
{
  Object_s *oObject = (Object_s *)obj; 
  Class oClass = getClass (oObject);
  COMclass cClass = SD_COM_FIND_CLASS_COM (oClass);
  COMobject cLastObj = SD_COM_FIND_OBJECT_COM (oObject);
  COMobject cNewObj = comEnv->COMcreateComponent (cClass);
  COMobject cDirObj = cDirObj = comEnv->COMnormalize (cNewObj);
  ObjectEntry *entry;

  avl_delete (swarmDirectory->COM_tree, COM_FIND_OBJECT_ENTRY (cLastObj));
  avl_probe (swarmDirectory->COM_tree, COM_OBJECT_ENTRY (cDirObj, oObject));

  entry = oObject->foreignEntry;
  entry->foreignObject.COM = (COMOBJECT) cDirObj;
  return cNewObj;
}

static ObjectEntry *
swarm_directory_COM_find (COMobject cObject)
{
  cObject = comEnv->COMnormalize (cObject);
  return (cObject
          ? avl_find (swarmDirectory->COM_tree,
                      COM_FIND_OBJECT_ENTRY (cObject))
          : nil);
}

id
swarm_directory_COM_find_object_objc (COMobject cObject)
{
  if (!cObject)
    return nil;
  else
    {
      ObjectEntry *result = swarm_directory_COM_find (cObject);

      return (result
              ? (id)result->object
              : nil);
    }
}

static ObjectEntry *
swarm_directory_COM_find_class (COMclass cClass)
{
  return (cClass
          ? avl_find (swarmDirectory->COM_tree,
                      COM_FIND_OBJECT_ENTRY (cClass))
          : nil);
}

Class
swarm_directory_COM_find_class_objc (COMclass cClass)
{
  if (!cClass)
    return Nil;
  else
    {
      ObjectEntry *result = swarm_directory_COM_find_class (cClass);

      return (result
              ? (id)result->object
              : nil);
    }
}

id
swarm_directory_COM_ensure_object_objc (COMobject cObject)
{
  if (!cObject)
    return nil;
  else
    {
      ObjectEntry *result;

      result = swarm_directory_COM_find (cObject);

      return (result
              ? (id)result->object
              : SD_COM_ADD_OBJECT_OBJC (comEnv->COMnormalize (cObject),
                                        [COMProxy create: globalZone]));
    }
}

SEL
swarm_directory_COM_ensure_selector (COMselector cSel)
{
  SEL sel = NULL;

  if (!cSel)
    sel = NULL;
  else if (!(sel = (SEL) SD_COM_FIND_OBJECT_OBJC (cSel)))
    {
      unsigned argCount = comEnv->selectorArgCount (cSel);
      const char *name = comEnv->selectorName (cSel);
      {
        unsigned ti;
        char signatureBuf[(argCount + 3) * 3 + 1], *p = signatureBuf;
        
        void add_type (fcall_type_t type)
          {
            const char *objcType = objc_type_for_fcall_type (type);

            p = stpcpy (p, objcType);
            *p++ = '0';
            *p = '\0';
            [globalZone free: (void *) objcType];
          }
        if (comEnv->selectorIsVoidReturn (cSel))
          add_type (fcall_type_void);
        else
          add_type (comEnv->selectorArgFcallType (cSel, argCount));
        add_type (fcall_type_object);
        add_type (fcall_type_selector);

        for (ti = 0; ti < argCount; ti++)
          add_type (comEnv->selectorArgFcallType (cSel, ti));

        sel = swarm_sel_getUidWithType (name);
        {
          BOOL needSelector = NO;
          
          if (sel)
            {
              if (!swarm_sel_getTypedUid (name, signatureBuf))
                {
#if 1
                  raiseEvent (WarningMessage,
                              "Method `%s' type (%s) differs from Swarm "
                              "method's type (%s)\n",
			      name, signatureBuf, swarm_sel_getTypeEncoding (sel));
#endif
                  needSelector = YES;
                }
              
            }
          else
            needSelector = YES;
          
          if (needSelector)
            {
#if SWARM_OBJC_DONE
              const char *type =
                mframe_build_signature (signatureBuf, NULL, NULL, NULL);
              
              sel = swarm_sel_registerTypedName (name, type);
#else
              sel = swarm_sel_registerName (name);
#endif
            }
        }
      }
      SD_COM_ADD_SELECTOR (cSel, sel);

      // Does GetName return a malloced pointer?
      // SFREEBLOCK (name); 
    }
  return sel;
}


Class
swarm_directory_COM_ensure_class_objc (COMclass cClass)
{
  Class objcClass;

  if (!(objcClass = SD_COM_FIND_CLASS_OBJC (cClass)))
    {
      const char *className = COM_get_class_name (cClass);

      if (strncmp (className, "swarm", 5) == 0)
        className += 5;
      
      objcClass = objc_class_for_class_name (className);
      
      if (objcClass == nil)
        objcClass = [COMProxy create: globalZone];
      (void) SD_COM_ADD_CLASS_COM (cClass, objcClass);
    }
  return objcClass;
}

static ObjectEntry *
add (COMobject cObject, Object_s *oObject)
{
  ObjectEntry *entry = COM_OBJECT_ENTRY (cObject, oObject);

  oObject->foreignEntry = entry;

  avl_probe (swarmDirectory->COM_tree, entry);
  return entry;
}

COMclass
swarm_directory_COM_add_class_COM (COMclass cClass, Class oClass)
{
  ObjectEntry *entry = COM_OBJECT_ENTRY (cClass, oClass);
  
  avl_probe (swarmDirectory->class_tree, entry);
  avl_probe (swarmDirectory->COM_tree, entry);

  return cClass;
}

COMobject
swarm_directory_COM_add_object_COM (COMobject cObject, id oObject)
{
  return add (cObject, oObject)->foreignObject.COM;
}

id
swarm_directory_COM_add_object_objc (COMobject cObject, id oObject)
{
  return add (cObject, oObject)->object;
}

void
swarm_directory_COM_add_selector (COMselector cSel, SEL oSel)
{
  SelectorEntry *entry = COM_SELECTOR_ENTRY (cSel, oSel);

  avl_probe (swarmDirectory->selector_tree, entry);
  avl_probe (swarmDirectory->COM_tree, entry);
}

