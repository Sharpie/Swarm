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

extern "C" {

static PRBool
findIID (const char *interfaceName, nsIID &iid)
{
  nsIInterfaceInfoManager *iim = nsnull;
  nsIEnumerator *Interfaces = nsnull;
  nsISupports *is_Interface;
  nsIInterfaceInfo *Interface;
  nsresult rv;
  PRBool matched = PR_FALSE;

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
          char *name;

          Interface->GetName (&name);

          printf ("%s/%s\n", interfaceName, name);
          matched = (PL_strcmp (interfaceName, name) == 0);
          iid = Interface->GetIID ();
          nsMemory::Free (name);
          NS_RELEASE (Interface);
        }
      else
        abort ();

      if (matched)
        break;

      NS_RELEASE (is_Interface);
      Interfaces->Next ();
      
    } while (NS_COMFALSE == Interfaces->IsDone ());
  
 done:
  NS_IF_RELEASE (Interfaces);
  NS_IF_RELEASE (iim);
  NS_IF_RELEASE (is_Interface);

  return matched;
}

static nsISupports *
createComponentByName (const char *progID, const char *interfaceName)
{
  nsISupports *obj;
  nsresult rv;
  nsIID iid;
  char buf[6 + PL_strlen (interfaceName) + 1];

  PL_strcpy (buf, "swarmI");
  PL_strcat (buf, interfaceName);

  printf ("progID: `%s' interfaceName: `%s'\n", progID, buf);
  if (findIID (buf, iid) != PR_TRUE)
    abort ();
  printf ("ok\n");

  rv = nsComponentManager::CreateInstance (progID, NULL, iid, (void **) &obj);
  if (NS_FAILED (rv))
    abort ();
  return obj;
}

void *
findComponent (const char *className)
{
  nsCID *cClass = new nsCID ();
  const char *prefix = "component://";
  char buf[12 + strlen (className) + 1];
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


}
