#import <defobj/COM.h>
#import <defobj/directory.h>
#import <defobj/COMProxy.h>

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
swarm_COM_copy_string (const char *str)
{
  return comEnv->copyString (str);
}

COMobject 
swarm_directory_objc_find_object_COM (id oObject)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (oObject);

  if (entry)
    {
      if (entry->type != foreign_COM)
        abort ();
      return entry->foreignObject.COM;
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
swarm_directory_objc_ensure_COM (id oObject)
{
  COMobject cObject;

  if (!oObject)
    return 0;
  
  cObject = SD_COM_FIND_OBJECT_COM (oObject);
  if (!cObject)
    {
      Class oClass = getClass (oObject);
      COMclass cClass = SD_COM_FIND_CLASS_COM (oClass);
      
      cObject = SD_COM_ADD_OBJECT_COM (comEnv->createComponent (cClass), oObject);
    }
  return cObject;
}

static ObjectEntry *
swarm_directory_COM_find (COMobject cObject)
{
  return (cObject
          ? avl_find (swarmDirectory->COM_tree,
                      COM_FIND_OBJECT_ENTRY (cObject))
          : nil);
}

id
swarm_directory_COM_ensure_objc (COMobject cObject)
{
  if (!cObject)
    return nil;
  else
    {
      ObjectEntry *result = swarm_directory_COM_find (cObject);

      return (result
              ? result->object
              : SD_COM_ADD_OBJECT_OBJC (cObject,
                                        [COMProxy create: globalZone]));
    }
}

SEL
swarm_directory_COM_ensure_selector (COMobject cSelector)
{
  abort ();
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

SelectorEntry *
swarm_directory_COM_add_selector (COMobject cSel, SEL sel)
{
  SelectorEntry *entry = COM_SELECTOR_ENTRY (cSel, sel);

  avl_probe (swarmDirectory->selector_tree, entry);
  return entry;
}

