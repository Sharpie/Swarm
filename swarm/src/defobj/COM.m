#import <defobj/COM.h>
#import <defobj/directory.h>

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

id
swarm_directory_COM_ensure_objc (COMobject cObject)
{
  abort ();
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

COMobject
swarm_directory_COM_add_COM (COMobject cObject, id oObject)
{
  return 0;
}
