#import <defobj/COM.h>
#import <defobj/directory.h>

COMobject 
swarm_directory_objc_find_object_COM (id object)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (object);

  if (entry)
    {
      if (entry->type != foreign_COM)
        abort ();
      return entry->foreignObject.COM;
    }
  return NULL;
}

static COMclass
COM_find_class (const char *name)
{
  return 0;
}

static COMclass
find_COM_wrapper_class (Class oClass)
{
  const char *name = language_independent_class_name_for_objc_class (oClass);
  COMclass cClass = COM_find_class (name);

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
        SD_COM_ADD_OBJECT_COM ((COMobject) cClass, oClass);
    }
  return cClass;
}

COMobject
swarm_directory_objc_ensure_COM (id object)
{
  abort ();
}

id
swarm_directory_COM_ensure_objc (COMobject cobj)
{
  abort ();
}

SEL
swarm_directory_COM_ensure_selector (COMobject csel)
{
  abort ();
}

Class
swarm_directroy_COM_ensure_class (COMclass clazz)
{
  abort ();
}

COMobject
swarm_directory_COM_add_COM (COMobject cObj, id oObj)
{
  abort ();
}
