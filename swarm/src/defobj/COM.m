#import "COM.h"
#import <defobj/directory.h>
#import <defobj/COMProxy.h>
#import <objc/mframe.h> // mframe_build_signature
#import "internal.h" // objc_type_for_fcall_type

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
  return comEnv->copyString (str);
}

const char *
COM_class_name (COMobject cObj)
{
  return comEnv->getName (cObj);
}

BOOL
COM_selector_is_boolean_return (COMselector cSel)
{
  return comEnv->selectorIsBooleanReturn (cSel);
}

void
COM_selector_invoke (COMselector cSel, void *args)
{
  comEnv->selectorInvoke (cSel, args);
}

void *
COM_create_arg_vector (unsigned size)
{
  return comEnv->createArgVector (size);
}

void
COM_set_arg (void *args, unsigned pos, fcall_type_t type, types_t *value)
{
  comEnv->setArg (args, pos, type, value);
}

void
COM_set_return (void *args, unsigned pos, fcall_type_t type, void *value)
{
  comEnv->setReturn (args, pos, type, value);
}

void
COM_free_arg_vector (void *args)
{
  comEnv->freeArgVector (args);
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
find_COM_wrapper_class (Class oClass)
{
  const char *name = language_independent_class_name_for_objc_class (oClass);
  COMclass cClass = comEnv->findComponent (name);

  FREECLASSNAME (name);
  return cClass;
}

COMclass
swarm_directory_objc_find_COM_class (Class oClass)
{
  COMclass cClass = SD_COM_FIND_OBJECT_COM (oClass);

  if (!cClass)
    {
      cClass = find_COM_wrapper_class (oClass);
      if (cClass)
        SD_COM_ADD_CLASS_COM (cClass, oClass);
    }
  return cClass;
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
      
      cObject = comEnv->normalize (comEnv->createComponent (cClass));
      return SD_COM_ADD_OBJECT_COM (cObject, oObject);
    }
  return cObject;
}

COMobject
swarm_directory_update_phase_COM (id oObject)
{
  Class oClass = getClass (oObject);
  COMclass cClass = SD_COM_FIND_CLASS_COM (oClass);
  COMobject cLastObj = SD_COM_FIND_OBJECT_COM (oObject);
  COMobject cNewObj = comEnv->createComponent (cClass);
  COMobject cDirObj = cDirObj = comEnv->normalize (cNewObj);
  ObjectEntry *entry;

  avl_delete (swarmDirectory->COM_tree, COM_FIND_OBJECT_ENTRY (cLastObj));
  avl_probe (swarmDirectory->COM_tree, COM_OBJECT_ENTRY (cDirObj, oObject));

  entry = avl_find (swarmDirectory->object_tree,
                    OBJC_FIND_OBJECT_ENTRY (oObject));

  entry->foreignObject.COM = (COMOBJECT) cDirObj;
  return cNewObj;
}

static ObjectEntry *
swarm_directory_COM_find (COMobject cObject)
{
  cObject = comEnv->normalize (cObject);
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
              ? result->object
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
              ? result->object
              : SD_COM_ADD_OBJECT_OBJC (comEnv->normalize (cObject),
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
        char signatureBuf[(argCount + 3) * 2 + 1], *p = signatureBuf;
        
        void add_type (fcall_type_t type)
          {
            const char *objcType =
              objc_type_for_fcall_type (type);

            *p++ = *objcType;
            *p++ = '0';
            *p = '\0';
            [globalZone free: (void *) objcType];
          }
        if (comEnv->selectorIsVoidReturn (cSel))
          add_type (fcall_type_void);
        else
          add_type (comEnv->selectorArgFcallType (cSel, argCount - 1));
        add_type (fcall_type_object);
        add_type (fcall_type_selector);

        for (ti = 0; ti < argCount - 1; ti++)
          add_type (comEnv->selectorArgFcallType (cSel, ti));

        sel = sel_get_any_typed_uid (name);
        {
          BOOL needSelector = NO;
          
          if (sel)
            {
              if (!sel_get_typed_uid (name, signatureBuf))
                {
#if 1
                  raiseEvent (WarningMessage,
                              "Method `%s' type (%s) differs from Swarm "
                              "method's type (%s)\n",
                            name, signatureBuf, sel->sel_types);
#endif
                  needSelector = YES;
                }
              
            }
          else
            needSelector = YES;
          
          if (needSelector)
            {
              const char *type =
                mframe_build_signature (signatureBuf, NULL, NULL, NULL);
              
              sel = sel_register_typed_name (name, type);
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
swarm_directory_COM_ensure_class (COMclass cClass)
{
  abort ();
}

static ObjectEntry *
add (COMobject cObject, id oObject)
{
  ObjectEntry *entry = COM_OBJECT_ENTRY (cObject, oObject);

  avl_probe (swarmDirectory->object_tree, entry);
  avl_probe (swarmDirectory->COM_tree, entry);
  return entry;
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

