#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include "nsIInterfaceInfoManager.h"
#include "nsIComponentManager.h"
#include "nsIEnumerator.h"
#include "nsMemory.h"
#include "xptinfo.h"
#include "COM.h"
#include "COMsupport.h"
#include "plstr.h"

#include "swarmITyping.h"
#include "swarmIZone.h"

extern "C" {

static void *
find (void *(*match) (nsIInterfaceInfo *, void *), void *item)
{
  nsIInterfaceInfoManager *iim = nsnull;
  nsIEnumerator *Interfaces = nsnull;
  nsISupports *is_Interface;
  nsIInterfaceInfo *Interface;
  nsresult rv;
  void *ret = NULL;
  
  if (!(iim = XPTI_GetInterfaceInfoManager ()))
    {
      NS_ASSERTION (0, "failed to get the InterfaceInfoManager");
      goto done;
    }
  
  if (NS_FAILED (iim->EnumerateInterfaces (&Interfaces)))
    {
      NS_ASSERTION (0, "failed to get interface enumeration");
      goto done;
    }
  
  if (NS_FAILED (rv = Interfaces->First ()))
    {
      NS_ASSERTION (0, "failed to go to first item in interface enumeration");
      goto done;
    }
  
  do
    {
      if (NS_FAILED (rv = Interfaces->CurrentItem (&is_Interface)))
        {
          /* maybe something should be done,
           * debugging info at least? */
          Interfaces->Next ();
          continue;
        }
      
      rv = is_Interface->
        QueryInterface (NS_GET_IID (nsIInterfaceInfo),
                        (void **) &Interface);
      
      if (!NS_FAILED (rv))
        {
          ret = (*match) (Interface, item);
          NS_RELEASE (Interface);
        }
      else
        abort ();
      
      if (ret)
        break;

      NS_RELEASE (is_Interface);
      Interfaces->Next ();
      
    } while (NS_COMFALSE == Interfaces->IsDone ());
  
 done:
  NS_IF_RELEASE (Interfaces);
  NS_IF_RELEASE (iim);
  NS_IF_RELEASE (is_Interface);
  
  return ret;
}

static void *
matchInterfaceName (nsIInterfaceInfo *interface, void *item)
{
  nsIID *ret = NULL;
  char *name;
  const char *interfaceName = (const char *)item;
  
  interface->GetName (&name);
  
  if (PL_strcmp (interfaceName, name) == 0)
    interface->GetIID (&ret);
  nsMemory::Free (name);
  return ret;
}

static nsIID *
findIIDFromName (const char *interfaceName)
{
  return (nsIID *) find (matchInterfaceName, (void *) interfaceName);
}

static void*
matchIID (nsIInterfaceInfo *interface, void *item)
{
  nsIID *iid = (nsIID *) item;
  nsIID *miid;
  
  interface->GetIID (&miid);
  
  if (iid->Equals (*miid))
    {
      char *name;
      interface->GetName (&name);
      
      return name;
    }
  return NULL;
}

static const char *
findNameFromIID (nsIID *iid)
{
  return (const char *) find (matchIID, (void *) iid);
}


static nsISupports *
createComponentByName (const char *progID, const char *interfaceName)
{
  nsISupports *obj;
  nsresult rv;
  nsIID *iid;
  char buf[6 + PL_strlen (interfaceName) + 1];

  PL_strcpy (buf, "swarmI");
  PL_strcat (buf, interfaceName);

  if (!(iid = findIIDFromName (buf)))
    abort ();

  rv = nsComponentManager::CreateInstance (progID, NULL, *iid, (void **) &obj);
  if (NS_FAILED (rv))
    abort ();
  return obj;
}

void *
findComponent (const char *className)
{
  nsCID *cClass = new nsCID ();
  const char *prefix = "component://";
  char buf[12 + PL_strlen (className) + 1];
  nsresult rv;

  PL_strcpy (buf, prefix);
  PL_strcat (buf, className);
  rv = nsComponentManager::ProgIDToClassID (buf, cClass);

  if (NS_FAILED (rv))
    abort ();
  return (void *) cClass;
}

void *
createComponent (COMclass cClass)
{
  nsCID *cid = (nsCID *) cClass;
  char *className;
  char *progID;
  char *interfaceName = NULL;
  nsresult rv = nsComponentManager::CLSIDToProgID (cid, &className, &progID);
  size_t len;
  nsISupports *obj;

  if (NS_FAILED (rv))
    abort ();
  
  len = PL_strlen (className);

  if (len > 10) /* swarm + name + Impl */
    {
      if (PL_strcmp (className + len - 4, "Impl") == 0)
        {
          interfaceName = PL_strdup (className + 5);
          interfaceName[len - 4 - 5] = '\0';
        }
      else
        abort ();
    }
  else
    abort ();

  obj = createComponentByName (progID, interfaceName);
  
  if (interfaceName)
    PL_strfree (interfaceName);

  return (void *) obj;
}

const char *
copyString (const char *str)
{
  const char *ret = (const char *)
    nsMemory::Clone (str, sizeof (char) * (PL_strlen (str) + 1));

  if (!ret)
    abort ();
  return ret;
}

const char *
getName (COMobject cObj)
{
  nsresult rv;
  char *name, *progID;
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCID *cid;
  swarmITyping *typing;

  rv = obj->QueryInterface (NS_GET_IID (swarmITyping), (void **) &typing);
  if (NS_FAILED (rv))
    abort ();
  
  rv = typing->GetCid (&cid);
  if (NS_FAILED (rv))
    abort ();

  rv = nsComponentManager::CLSIDToProgID (cid, &name, &progID);
  if (NS_FAILED (rv))
    abort ();

  NS_RELEASE (typing);

  return name;
}

void
addRef (COMobject cObj)
{
  NS_ADDREF (NS_STATIC_CAST (nsISupports *, cObj));
}

}
