#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include "nsIInterfaceInfoManager.h"
#include "nsIComponentManager.h"
#include "nsIEnumerator.h"
#include "nsMemory.h"
#include "xptinfo.h"
#include "xptcall.h"
#include "COM.h"
#include "COMsupport.h"
#include "plstr.h"

#include "swarmITyping.h"
#include "swarmIZone.h"

extern "C" {

struct method_key {
  nsISupports *target;
  const char *methodName;
};

struct method_value {
  nsISupports *interface;
  PRUint16 methodIndex;
  const nsXPTMethodInfo *methodInfo;
};

struct method_pair {
  struct method_key key;
  struct method_value value;
};

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
          if (ret != Interface)
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
matchInterfaceName (nsIInterfaceInfo *interfaceInfo, void *item)
{
  nsIID *ret = NULL;
  char *name;
  const char *interfaceName = (const char *)item;
  
  interfaceInfo->GetName (&name);
  
  if (PL_strcmp (interfaceName, name) == 0)
    interfaceInfo->GetIID (&ret);
  nsMemory::Free (name);
  return ret;
}

static nsIID *
findIIDFromName (const char *interfaceName)
{
  return (nsIID *) find (matchInterfaceName, (void *) interfaceName);
}

static void *
matchMethodName (nsIInterfaceInfo *interfaceInfo, void *item)
{
  struct method_pair *pair = (struct method_pair *) item;
  nsISupports *interface;
  nsIID *iid;

  if (interfaceInfo->GetIID (&iid) != NS_OK)
    abort ();

  if (pair->key.target->QueryInterface (*iid, (void **) &interface) == NS_OK)
    {
      if (NS_SUCCEEDED (interfaceInfo->GetMethodInfoForName
                        (pair->key.methodName,
                         &pair->value.methodIndex,
                         &pair->value.methodInfo)))
        {
          pair->value.interface = interface;
          return (void *) pair;
        }
    }
return NULL;
}

static nsISupports *
createComponentByName (const char *contractID, const char *interfaceName)
{
  nsISupports *obj;
  nsresult rv;
  nsIID *iid;
  char buf[6 + PL_strlen (interfaceName) + 1];

  PL_strcpy (buf, "swarmI");
  PL_strcat (buf, interfaceName);

  if (!(iid = findIIDFromName (buf)))
    abort ();

  rv = nsComponentManager::CreateInstance (contractID, NULL, *iid, (void **) &obj);
  if (NS_FAILED (rv))
    abort ();
  return obj;
}

void *
findComponent (const char *className)
{
  const char *prefix = "urn:";
  const char *modulePrefix = "swarm/";
  size_t prefixLen = PL_strlen (prefix);
  size_t modulePrefixLen = PL_strlen (modulePrefix);
  nsCID *cClass = new nsCID ();
  size_t classNameLen = PL_strlen (className);
  char buf[prefixLen + classNameLen + 1];
  nsresult rv;

  PL_strcpy (buf, prefix);
  PL_strcat (buf, className);
  if (PL_strncmp (className, modulePrefix, modulePrefixLen) == 0)
    {
      unsigned i;

      buf[prefixLen + 5] = ':';
      for (i = modulePrefixLen; i < classNameLen; i++)
        {
          unsigned pos = prefixLen + i;
          
          if (buf[pos] == '/')
            buf[pos] = '.';
        }
    }

  rv = nsComponentManager::ContractIDToClassID (buf, cClass);

  if (NS_FAILED (rv))
    abort ();
  return (void *) cClass;
}

void *
createComponent (COMclass cClass)
{
  nsCID *cid = (nsCID *) cClass;
  char *className;
  char *contractID;
  char *interfaceName = NULL;
  nsresult rv = nsComponentManager::CLSIDToContractID (cid, &className, &contractID);
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

  obj = createComponentByName (contractID, interfaceName);
  
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
  char *name, *contractID;
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCID *cid;
  swarmITyping *typing;

  rv = obj->QueryInterface (NS_GET_IID (swarmITyping), (void **) &typing);
  if (NS_FAILED (rv))
    abort ();
  
  rv = typing->GetCid (&cid);
  if (NS_FAILED (rv))
    abort ();

  rv = nsComponentManager::CLSIDToContractID (cid, &name, &contractID);
  if (NS_FAILED (rv))
    abort ();

  NS_RELEASE (typing);

  return name;
}

COMobject
normalize (COMobject cObj)
{
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, cObj);
  swarmITyping *typing;

  if (!NS_SUCCEEDED (obj->QueryInterface (NS_GET_IID (swarmITyping), (void **) &typing)))
    abort ();
  
  return typing;
}

COMselector
selectorQuery (COMobject cObj)
{
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, cObj);
  swarmITyping *selector;

  if (!NS_SUCCEEDED (obj->QueryInterface (NS_GET_IID (swarmISelector), (void **) &selector)))
    abort ();
  return selector;
}

BOOL
selectorIsVoidReturn (COMselector cSel)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  PRBool ret;
  
  if (!NS_SUCCEEDED (sel->IsVoidReturn (&ret)))
    abort ();
  
  return ret;
}

BOOL
selectorIsBooleanReturn (COMselector cSel)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  PRBool ret;

  if (!NS_SUCCEEDED (sel->IsBooleanReturn (&ret)))
    abort ();
  
  return ret;
}

const char *
selectorName (COMselector cSel)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  char *ret;

  if (!NS_SUCCEEDED (sel->GetName (&ret)))
    abort ();

  return ret;
}

unsigned
selectorArgCount (COMselector cSel)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  unsigned ret;

  if (!NS_SUCCEEDED (sel->GetArgCount (&ret)))
    abort ();
  
  return ret;
}

fcall_type_t
selectorArgFcallType (COMselector cSel, unsigned argIndex)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  unsigned short ret;

  if (!NS_SUCCEEDED (sel->GetArgFcallType (argIndex, &ret)))
    abort ();
  
  return (fcall_type_t) ret;
}

void
selectorInvoke (COMselector cSel, void *args)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  
  if (!NS_SUCCEEDED (sel->Invoke ((nsXPTCVariant *) args)))
    abort ();
}

}

PRBool
findMethod (nsISupports *obj, const char *methodName, nsISupports **interface, PRUint16 *index, const nsXPTMethodInfo **methodInfo)
{
  struct method_pair pair = {{ obj, methodName }};

  if (find (matchMethodName, &pair))
    {
      *index = pair.value.methodIndex;
      *interface = pair.value.interface;
      *methodInfo = pair.value.methodInfo;
      return PR_TRUE;
    }
  return PR_FALSE;
}

void *
createArgVector (unsigned size)
{
  nsXPTCVariant *argVec = new nsXPTCVariant[size];

  return (void *) argVec;
}

static nsXPTType types[FCALL_TYPE_COUNT] = {
  nsXPTType::T_VOID,
  nsXPTType::T_BOOL,
  nsXPTType::T_U8,
  nsXPTType::T_I8,
  nsXPTType::T_U16,
  nsXPTType::T_I16,
  nsXPTType::T_U32,
  nsXPTType::T_I32,
  nsXPTType::T_U32,
  nsXPTType::T_I32,
  nsXPTType::T_U64,
  nsXPTType::T_I64,
  nsXPTType::T_FLOAT,
  nsXPTType::T_DOUBLE,
  nsXPTType::T_VOID, // long double
  nsXPTType::T_INTERFACE,
  nsXPTType::T_IID,
  nsXPTType::T_CHAR_STR,
  nsXPTType::T_INTERFACE,
  nsXPTType::T_VOID, // jobject
  nsXPTType::T_VOID // jstring
};

void
setArg (void *args, unsigned pos, fcall_type_t type, types_t *value)
{
  nsXPTCVariant *arg = &((nsXPTCVariant *) args)[pos];

  switch (type)
    {
    case fcall_type_void:
      abort ();
    case fcall_type_boolean:
      arg->type = nsXPTType::T_BOOL;
      arg->val.b = value->boolean;
      break;
    case fcall_type_uchar:
      arg->type = nsXPTType::T_U8;
      arg->val.u8 = value->uchar;
      break;
    case fcall_type_schar:
      arg->type = nsXPTType::T_I8;
      arg->val.i8 = value->schar;
      break;
    case fcall_type_ushort:
      arg->type = nsXPTType::T_U16;
      arg->val.u16 = value->ushort;
      break;
    case fcall_type_sshort:
      arg->type = nsXPTType::T_I16;
      arg->val.i16 = value->sshort;
      break;
    case fcall_type_uint:
      arg->type = nsXPTType::T_U32;
      arg->val.u32 = value->uint;
      break;
    case fcall_type_sint:
      arg->type = nsXPTType::T_I32;
      arg->val.i32 = value->sint;
      break;
    case fcall_type_ulong:
      arg->type = nsXPTType::T_U32;
      arg->val.u32 = value->ulong;
      break;
    case fcall_type_slong:
      arg->type = nsXPTType::T_I32;
      arg->val.i32 = value->slong;
      break;
    case fcall_type_ulonglong:
      arg->type = nsXPTType::T_U64;
      arg->val.u64 = value->ulonglong;
      break;
    case fcall_type_slonglong:
      arg->type = nsXPTType::T_I64;
      arg->val.i64 = value->slonglong;
      break;
    case fcall_type_float:
      arg->type = nsXPTType::T_FLOAT;
      arg->val.f = value->_float;
      break;
    case fcall_type_double:
      arg->type = nsXPTType::T_DOUBLE;
      arg->val.d = value->_double;
      break;
    case fcall_type_long_double:
      abort ();
    case fcall_type_object:
      arg->type = nsXPTType::T_INTERFACE;
      arg->val.p = SD_COM_ENSURE_OBJECT_COM (value->object);
      break;
    case fcall_type_string:
      arg->type = nsXPTType::T_CHAR_STR;
      arg->val.p = (void *) value->string;
      break;
    case fcall_type_selector:
    case fcall_type_jobject:
    case fcall_type_jstring:
    default:
      abort ();
    }
}

void
setReturn (void *args, unsigned pos, fcall_type_t type, void *value)
{
  nsXPTCVariant *retArg = &((nsXPTCVariant *) args)[pos];

  retArg->ptr = value;
  retArg->type = types[type];
  retArg->flags = nsXPTCVariant::PTR_IS_DATA;
}

void
freeArgVector (void *args)
{
  nsXPTCVariant *argVec = (nsXPTCVariant *) args;

  delete argVec;
}
