#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include "nsIInterfaceInfoManager.h"
#include "nsIComponentManager.h"
#include "nsIServiceManager.h"
#include "nsIXPConnect.h"
#include "nsIEnumerator.h"
#include "nsMemory.h"
#include "xptinfo.h"
#include "xptcall.h"
#include "COM.h"
#include "COMsupport.h"
#include "plstr.h"
#include "nsHashtable.h"

#include "swarmIBase.h"
#include "swarmITyping.h"
#include "swarmIZone.h"

#include "componentIDs.h"

#include "jsapi.h"

struct method_key {
  nsISupports *target;
  const char *methodName;
};

struct method_pair {
  struct method_key key;
  struct method_value value;
};

struct collect_methods_t {
  nsISupports *obj;
  COM_collect_variable_func_t collectVariableFunc;
  COM_collect_method_func_t collectMethodFunc;
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
  const char *interfaceName = (const char *) item;
  
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
          pair->value.iid = iid;
          return (void *) pair;
        }
      NS_RELEASE (interface);
    }
return NULL;
}

static nsISupports *
createComponentByName (const char *contractID, const char *interfaceName)
{
  nsISupports *obj;
  nsresult rv;
  nsIID *iid;
  nsIID default_iid = NS_GET_IID (nsISupports);

  if (interfaceName)
    {
      char buf[6 + PL_strlen (interfaceName) + 1];

      PL_strcpy (buf, "swarmI");
      PL_strcat (buf, interfaceName);
      
      if (!(iid = findIIDFromName (buf)))
        iid = &default_iid;
    }
  else
    iid = &default_iid;
  
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
        interfaceName = NULL;
    }
  else
    interfaceName = NULL;

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
  nsISupports *_cObj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCOMPtr <swarmITyping> typing (do_QueryInterface (_cObj));

  if (!typing)
    {
      nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (_cObj));

      if (jsObj)
        return "[wrapped JavaScript]";
      else
        abort ();
    }
  else
    {
      nsCID *cid;

      rv = typing->GetCid (&cid);
      if (NS_FAILED (rv))
        abort ();

      return getComponentName (cid);
    }
}

const char *
getComponentName (COMclass cClass)
{
  char *name, *contractID;
  nsCID *cid = (nsCID *) cClass;

  if (NS_FAILED (nsComponentManager::CLSIDToContractID (cid, &name, &contractID)))
    abort ();

  return name;
}

COMclass
copyComponentID (COMclass cClass)
{
  nsCID *cid = new nsCID ();

  *cid = *((nsCID *) cClass);
  return (COMclass) cid;
}

COMobject
normalize (COMobject cObj)
{
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, cObj);
  swarmITyping *typing;

  if (NS_SUCCEEDED (obj->QueryInterface (NS_GET_IID (swarmITyping), (void **) &typing)))
    return typing;
  else
    // e.g. JavaScript objects
    {
      nsISupports *baseInterface;
     
      if (NS_SUCCEEDED (obj->QueryInterface (NS_GET_IID (nsISupports), (void **) &baseInterface)))
        return baseInterface;
      else
        abort ();
    }
}

static NS_DEFINE_CID (kSelector, SWARM_SELECTOR_CID);

COMselector
selectorCreate (COMmethod cMethod)
{
  swarmISelector *cSel, *ret;

  if (!NS_SUCCEEDED (nsComponentManager::CreateInstance (kSelector, nsnull,
                                                         NS_GET_IID (swarmISelector),
                                                         (void **) &cSel)))
    abort ();
  if (!NS_SUCCEEDED (cSel->CreateFromMethod (cMethod, &ret)))
    abort ();
  return cSel;
}

COMselector
selectorQuery (COMobject cObj)
{
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, cObj);
  swarmISelector *selector;

  if (!NS_SUCCEEDED (obj->QueryInterface (NS_GET_IID (swarmISelector), (void **) &selector)))
    abort ();
  return selector;
}

BOOL
selectorIsJavaScript (COMselector cSel)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  PRBool ret;
  
  if (!NS_SUCCEEDED (sel->IsJavaScript (&ret)))
    abort ();
  
  return ret;
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
selectorCOMInvoke (COMselector cSel, COMobject cObj, void *params)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
  
  if (!NS_SUCCEEDED (sel->COMinvokeX (NS_STATIC_CAST (nsISupports *,cObj),
                                      (nsXPTCVariant *) params)))
    abort ();
}

void
selectorJSInvoke (COMselector cSel, COMobject cObj, void *params)
{
  swarmISelector *sel = NS_STATIC_CAST (swarmISelector *, cSel);
 
  if (!NS_SUCCEEDED (sel->JSinvokeX (NS_STATIC_CAST (nsISupports *,cObj),
                                     (jsval *) params)))
    abort ();
}

PRBool
findMethod (nsISupports *obj, const char *methodName, nsIID **iid, PRUint16 *index, const nsXPTMethodInfo **methodInfo)
{
  struct method_pair pair = {{ obj, methodName }};

  if (find (matchMethodName, &pair))
    {
      *index = pair.value.methodIndex;
      *iid = pair.value.iid;
      *methodInfo = pair.value.methodInfo;
      return PR_TRUE;
    }
  return PR_FALSE;
}

static PRBool
enumCollectFunc (nsHashKey *key, void *data, void *param)
{
  COM_collect_method_func_t collectFunc = (COM_collect_method_func_t) param;

  collectFunc ((struct method_value *) data);

  return PR_TRUE;
}

static PRBool
destroyMethod (nsHashKey *key, void *data, void *param)
{
  // struct method_value *value = (struct method_value *) data;
  // delete value;
  return PR_TRUE;
}

static void *
matchImplementedInterfaces (nsIInterfaceInfo *interfaceInfo, void *item)
{
  struct collect_methods_t *info = (struct collect_methods_t *) item;
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, info->obj);
  nsIID *iid;

  if (info->collectVariableFunc && info->collectMethodFunc)
    abort ();

  if (!NS_SUCCEEDED (interfaceInfo->GetIID (&iid)))
    abort ();

  nsISupports *interface;

  if (NS_SUCCEEDED (obj->QueryInterface (*iid, (void **) &interface)))
    {
      PRUint16 methodCount, i;
      const nsXPTMethodInfo *methodInfo;

      if (!NS_SUCCEEDED (interfaceInfo->GetMethodCount (&methodCount)))
        abort ();

      char *name;
      if (!NS_SUCCEEDED (interfaceInfo->GetName (&name)))
        abort ();

      nsObjectHashtable *ht = new nsObjectHashtable (nsnull, nsnull,
                                                     destroyMethod, nsnull);
      
      for (i = 0; i < methodCount; i++)
        {
          if (!NS_SUCCEEDED (interfaceInfo->GetMethodInfo (i, &methodInfo)))
            abort ();
          
          struct method_value *method = new method_value;
          
          if (!NS_SUCCEEDED (interfaceInfo->GetIID (&method->iid)))
            abort ();
          method->methodIndex = i;
          method->methodInfo = methodInfo;

          const char *variableName = methodInfo->GetName ();
          nsCStringKey *key = new nsCStringKey (variableName);
          struct method_value *lastMethod =
            (struct method_value *) ht->Get (key);
          
          if (lastMethod)
            {
              if (info->collectVariableFunc)
                {
                  if (methodInfo->IsGetter ()
                      && lastMethod->methodInfo->IsSetter ())
                    info->collectVariableFunc (method, lastMethod);
                  else if (methodInfo->IsSetter ()
                           && lastMethod->methodInfo->IsGetter ())
                    info->collectVariableFunc (lastMethod, method);
                }
              else
                {
                  ht->Remove (key);
                  delete method;
                }
            }
          else
            ht->Put (key, (void *) method);
        }
      if (info->collectMethodFunc)
        ht->Enumerate (&enumCollectFunc, (void *) info->collectMethodFunc);
      delete ht;
      NS_RELEASE (interface);
    }
  return NULL;
}


void
COMcollect (COMclass cClass,
            COM_collect_variable_func_t varFunc,
            COM_collect_method_func_t methodFunc)
{
  nsISupports *obj = NS_STATIC_CAST (nsISupports *, createComponent (cClass));
  struct collect_methods_t info = { obj, varFunc, methodFunc };
  if (!obj)
    abort ();
  find (matchImplementedInterfaces, &info);
  NS_RELEASE (obj);
}

void
JScollect (COMobject cObj,
           JS_collect_func_t variableFunc,
           JS_collect_func_t methodFunc)
{
  nsISupports *_cObj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (_cObj));

  if (!jsObj)
    abort ();

  JSObject *jsobj;
  if (!NS_SUCCEEDED (jsObj->GetJSObject (&jsobj)))
    abort ();

  JSIdArray *ida = JS_Enumerate (currentJSContext (),
                                 jsobj);
  
  if (!ida)
    abort ();
  
  jsint i;
  JSContext *cx = currentJSContext ();
  
  for (i = 0; i < ida->length; i++)
    {
      jsid _id = ida->vector[i];
      jsval val;
      
      if (!JS_IdToValue (cx, _id, &val))
        abort ();
      
      char *name = JS_GetStringBytes (JSVAL_TO_STRING (val));
      jsval propval;
      
      if (!JS_GetProperty (cx, jsobj, name, &propval))
        abort ();

      if (JS_ValueToFunction (cx, propval))
        {
          if (methodFunc)
            methodFunc (name);
        }
      else if (variableFunc)
        variableFunc (name);
    }
  JS_DestroyIdArray (cx, ida);
}

const char *
COMmethodName (COMmethod method)
{
  struct method_value *value = (struct method_value *) method;

  return value->methodInfo->GetName ();
}

unsigned
COMmethodArgCount (COMmethod method)
{
  struct method_value *value = (struct method_value *) method;
  const nsXPTMethodInfo *methodInfo = value->methodInfo;

  PRUint16 count = methodInfo->GetParamCount ();

  if (count > 0)
    {
      PRUint16 i = count - 1;
      const nsXPTParamInfo& param = methodInfo->GetParam (i);
      return count - (param.IsRetval () != 0);
    }
  else
    return 0;
}

fcall_type_t
methodParamFcallType (const nsXPTMethodInfo *methodInfo, PRUint16 paramIndex)
{
  const nsXPTParamInfo& param = methodInfo->GetParam (paramIndex);
  const nsXPTType& type = param.GetType ();
  fcall_type_t ret = fcall_type_void;

  switch (type.TagPart ())
    {
    case nsXPTType::T_BOOL:
      ret = fcall_type_boolean;
      break;
    case nsXPTType::T_I8:
      ret = fcall_type_schar;
      break;
    case nsXPTType::T_I16:
      ret = fcall_type_sshort;
      break;
    case nsXPTType::T_I32:
      ret = fcall_type_sint;
      break;
    case nsXPTType::T_I64:
      ret = fcall_type_slonglong;
      break;
    case nsXPTType::T_U8:
      ret = fcall_type_uchar;
      break;
    case nsXPTType::T_U16:
      ret = fcall_type_ushort;
      break;
    case nsXPTType::T_U32:
      ret = fcall_type_uint;
      break;
    case nsXPTType::T_U64:
      ret = fcall_type_ulonglong;
      break;
    case nsXPTType::T_FLOAT:
      ret = fcall_type_float;
      break;
    case nsXPTType::T_DOUBLE:
      ret = fcall_type_double;
      break;
    case nsXPTType::T_CHAR:
      ret = fcall_type_schar;
      break;
    case nsXPTType::T_VOID:
      ret = fcall_type_void;
      break;
    case nsXPTType::T_CHAR_STR:
      ret = fcall_type_string;
      break;
    case nsXPTType::T_INTERFACE:
    case nsXPTType::T_INTERFACE_IS:
      ret = fcall_type_object;
      break;
    case nsXPTType::T_IID:
      ret = fcall_type_iid;
      break;
      
    case nsXPTType::T_WCHAR:
    case nsXPTType::T_WCHAR_STR:
    case nsXPTType::T_ARRAY:
    case nsXPTType::T_PSTRING_SIZE_IS:
    case nsXPTType::T_PWSTRING_SIZE_IS:
      abort ();
    }
  return ret;
}

fcall_type_t
JSToFcallType (unsigned type)
{
  fcall_type_t ret;

  switch (type)
    {
    case JSVAL_OBJECT:
      ret = fcall_type_object;
      break;
    case JSVAL_INT:
      ret = fcall_type_sint;
      break;
    case JSVAL_DOUBLE:
      ret = fcall_type_double;
      break;
    case JSVAL_STRING:
      ret = fcall_type_string;
      break;
    case JSVAL_BOOLEAN:
      ret = fcall_type_boolean;
      break;
    default:
      abort ();
    }
  return ret;
}
  
fcall_type_t
COMmethodParamFcallType (COMmethod cMethod, unsigned paramIndex)
{
  struct method_value *method = (struct method_value *) cMethod;

  if (paramIndex >= method->methodInfo->GetParamCount ())
    abort ();

  return methodParamFcallType (method->methodInfo, paramIndex);
}

void
COMmethodSetReturn (COMmethod cMethod, void *params, void *value)
{
  struct method_value *method = (struct method_value *) cMethod;
  const nsXPTMethodInfo *methodInfo = method->methodInfo;
  PRUint16 paramCount = methodInfo->GetParamCount ();
  
  if (COMmethodArgCount (cMethod) < paramCount)
    {
      nsXPTCVariant &retParam = ((nsXPTCVariant *) params)[paramCount - 1];
      
      retParam.ptr = value;
      retParam.type = methodInfo->GetParam (paramCount - 1).GetType ();
      retParam.flags = nsXPTCVariant::PTR_IS_DATA;
    }
  else
    abort ();
}

void
COMmethodInvoke (COMmethod cMethod, COMobject obj, void *params)
{
  struct method_value *method = (struct method_value *) cMethod;
  nsISupports *_obj = NS_STATIC_CAST (nsISupports *, obj);
  nsISupports *interface;

  if (!NS_SUCCEEDED (_obj->QueryInterface (*method->iid,
                                           (void **) &interface)))
    abort ();

  if (!NS_SUCCEEDED (XPTC_InvokeByIndex (interface,
                                         method->methodIndex,
                                         method->methodInfo->GetParamCount (),
                                         (nsXPTCVariant *) params)))
    abort ();
  NS_RELEASE (interface);
}

void *
COMcreateParams (unsigned size)
{
  nsXPTCVariant *params = new nsXPTCVariant[size];

  return (void *) params;
}


void
COMfreeParams (void *params)
{
  nsXPTCVariant *_params = (nsXPTCVariant *) params;

  delete _params;
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
  nsXPTType::T_IID, // class
  nsXPTType::T_CHAR_STR,
  nsXPTType::T_INTERFACE,
  nsXPTType::T_VOID, // jobject
  nsXPTType::T_VOID, // jstring
  nsXPTType::T_IID // iid
};

void
COMsetArg (void *params, unsigned pos, val_t *val)
{
  nsXPTCVariant *param = &((nsXPTCVariant *) params)[pos];
  
  switch (val->type)
    {
    case fcall_type_void:
      abort ();
    case fcall_type_boolean:
      param->type = nsXPTType::T_BOOL;
      param->val.b = val->val.boolean;
      break;
    case fcall_type_uchar:
      param->type = nsXPTType::T_U8;
      param->val.u8 = val->val.uchar;
      break;
    case fcall_type_schar:
      param->type = nsXPTType::T_I8;
      param->val.i8 = val->val.schar;
      break;
    case fcall_type_ushort:
      param->type = nsXPTType::T_U16;
      param->val.u16 = val->val.ushort;
      break;
    case fcall_type_sshort:
      param->type = nsXPTType::T_I16;
      param->val.i16 = val->val.sshort;
      break;
    case fcall_type_uint:
      param->type = nsXPTType::T_U32;
      param->val.u32 = val->val.uint;
      break;
    case fcall_type_sint:
      param->type = nsXPTType::T_I32;
      param->val.i32 = val->val.sint;
      break;
    case fcall_type_ulong:
      param->type = nsXPTType::T_U32;
      param->val.u32 = val->val.ulong;
      break;
    case fcall_type_slong:
      param->type = nsXPTType::T_I32;
      param->val.i32 = val->val.slong;
      break;
    case fcall_type_ulonglong:
      param->type = nsXPTType::T_U64;
      param->val.u64 = val->val.ulonglong;
      break;
    case fcall_type_slonglong:
      param->type = nsXPTType::T_I64;
      param->val.i64 = val->val.slonglong;
      break;
    case fcall_type_float:
      param->type = nsXPTType::T_FLOAT;
      param->val.f = val->val._float;
      break;
    case fcall_type_double:
      param->type = nsXPTType::T_DOUBLE;
      param->val.d = val->val._double;
      break;
    case fcall_type_long_double:
      abort ();
    case fcall_type_object:
      param->type = nsXPTType::T_INTERFACE;
      param->val.p = SD_COM_ENSURE_OBJECT_COM (val->val.object);
      break;
    case fcall_type_string:
      param->type = nsXPTType::T_CHAR_STR;
      param->val.p = (void *) val->val.string;
      break;
    case fcall_type_iid:
      param->type = nsXPTType::T_IID;
      param->val.p = val->val.iid;
      break;
    case fcall_type_class:
    case fcall_type_selector:
    case fcall_type_jobject:
    case fcall_type_jstring:
      abort ();

    }
}

void
COMsetReturn (void *params, unsigned pos, val_t *val)
{
  nsXPTCVariant &retParam = ((nsXPTCVariant *) params)[pos];

  retParam.ptr = &val->val;
  retParam.type = types[val->type];
  retParam.flags = nsXPTCVariant::PTR_IS_DATA;
}

JSContext *
currentJSContext ()
{
  nsresult rv;
  
  NS_WITH_SERVICE (nsIXPConnect, xpc, nsIXPConnect::GetCID (), &rv);
  if (!NS_SUCCEEDED (rv))
    abort ();
  
  nsCOMPtr <nsIXPCNativeCallContext> callContext;
  xpc->GetCurrentNativeCallContext (getter_AddRefs (callContext));
  if (!callContext)
    abort ();
  
  JSContext *cx;
  rv = callContext->GetJSContext (&cx);

  if (!NS_SUCCEEDED (rv))
    abort ();
  
  return cx;
}

JSObject *
currentJSObject ()
{
  nsresult rv;
  
  NS_WITH_SERVICE (nsIXPConnect, xpc, nsIXPConnect::GetCID (), &rv);
  if (!NS_SUCCEEDED (rv))
    abort ();
  
  nsCOMPtr <nsIXPCNativeCallContext> callContext;
  xpc->GetCurrentNativeCallContext (getter_AddRefs (callContext));
  if (!callContext)
    abort ();
  
  nsCOMPtr <nsIXPConnectWrappedNative> calleeWrapper;
  callContext->GetCalleeWrapper (getter_AddRefs (calleeWrapper));
  nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (calleeWrapper));

  JSObject *jsobj;
 if (!NS_SUCCEEDED (jsObj->GetJSObject (&jsobj)))
    abort ();
  return jsobj;
}

BOOL
isJavaScript (COMobject cObj)
{
  nsISupports *_cObj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (_cObj));
  
  return jsObj != NULL;
}

void *
JScreateParams (unsigned size)
{
  jsval *params =
    (jsval *) JS_malloc (currentJSContext (), sizeof (jsval) * size);
  
  return (void *) params;
}


static void
setJSval (val_t *inval, jsval *outval)
{
  JSContext *cx = currentJSContext ();

  switch (inval->type)
    {
    case fcall_type_void:
      abort ();
    case fcall_type_boolean:
      *outval = BOOLEAN_TO_JSVAL (inval->val.boolean);
      break;
    case fcall_type_uchar:
      *outval = INT_TO_JSVAL ((int) inval->val.uchar);
      break;
    case fcall_type_schar:
      *outval = INT_TO_JSVAL ((int) inval->val.schar);
      break;
    case fcall_type_ushort:
      *outval = INT_TO_JSVAL ((int) inval->val.ushort);
      break;
    case fcall_type_sshort:
      *outval = INT_TO_JSVAL ((int) inval->val.sshort);
      break;
    case fcall_type_uint:
      *outval = INT_TO_JSVAL ((int) inval->val.uint);
      break;
    case fcall_type_sint:
      *outval = INT_TO_JSVAL ((int) inval->val.sint);
      break;
    case fcall_type_ulong:
      *outval = INT_TO_JSVAL ((int) inval->val.ulong);
      break;
    case fcall_type_slong:
      *outval = INT_TO_JSVAL ((int) inval->val.slong);
      break;
    case fcall_type_ulonglong:
      *outval = INT_TO_JSVAL ((int) inval->val.ulonglong);
      break;
    case fcall_type_slonglong:
      *outval = INT_TO_JSVAL ((int) inval->val.slonglong);
      break;
    case fcall_type_float:
      if (!JS_NewDoubleValue (cx, (jsdouble) inval->val._float, outval))
        abort ();
      break;
    case fcall_type_double:
      if (!JS_NewDoubleValue (cx, (jsdouble) inval->val._double, outval))
        abort ();
      break;
    case fcall_type_long_double:
      if (!JS_NewDoubleValue (cx, (jsdouble) inval->val._long_double, outval))
        abort ();
      break;
    case fcall_type_object:
      {
        swarmITyping *cObject = SD_COM_ENSURE_OBJECT_COM (inval->val.object);
        nsCOMPtr <swarmIBase> base (do_QueryInterface (cObject));
        nsCOMPtr <nsIXPCNativeCallContext> callContext;
        base->GetNativeCallContext (getter_AddRefs (callContext));
        nsCOMPtr <nsIXPConnectWrappedNative> calleeWrapper;
        callContext->GetCalleeWrapper (getter_AddRefs (calleeWrapper));

        JSObject *jsObj;
        if (!NS_SUCCEEDED (calleeWrapper->GetJSObject (&jsObj)))
          abort ();
        *outval = OBJECT_TO_JSVAL (jsObj);
      }
      break;
    case fcall_type_selector:
      {
        nsCOMPtr <swarmISelector> cSel = NS_STATIC_CAST (swarmISelector *, SD_COM_FIND_SELECTOR_COM (inval->val.selector));
        nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (cSel));
        JSObject *jsobj;
        if (!NS_SUCCEEDED (jsObj->GetJSObject (&jsobj)))
          abort ();
        *outval = OBJECT_TO_JSVAL (jsobj);
      }
      break;
    case fcall_type_class:
      abort ();
      break;
    case fcall_type_string:
      *outval = STRING_TO_JSVAL (JS_NewStringCopyZ (currentJSContext (),
                                                    inval->val.string));
      break;
    case fcall_type_iid:
    case fcall_type_jobject:
    case fcall_type_jstring:
      abort ();
    }
}

void
JSsetArg (void *params, unsigned pos, val_t *inval)
{
  jsval *jsparams = (jsval *) params;

  setJSval (inval, &jsparams[pos]);
}

void
JSsetReturn (void *params, unsigned pos, val_t *inval)
{
  if (inval->type != fcall_type_void)
    JSsetArg (params, pos, inval);
}

void
JSfreeParams (void *params)
{
  JS_free (currentJSContext (), params);
}

BOOL
JSprobeVariable (COMobject cObj, const char *variableName, val_t *ret)
{
  nsISupports *_cObj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (_cObj));
  JSObject *jsobj;
  jsval val;
  JSContext *cx = currentJSContext ();

  if (!NS_SUCCEEDED (jsObj->GetJSObject (&jsobj)))
    abort ();

  if (!JS_LookupProperty (cx, jsobj, variableName, &val))
    return NO;

  if (JSVAL_IS_OBJECT (val))
    {
      nsresult rv;
      NS_WITH_SERVICE (nsIXPConnect, xpc, nsIXPConnect::GetCID (), &rv);
      if (!NS_SUCCEEDED (rv))
        abort ();

      nsISupports *holder;
      xpc->WrapJS (cx, JSVAL_TO_OBJECT (val), NS_GET_IID (nsISupports),
                   (void **) &holder);
      ret->type = fcall_type_object;
      ret->val.object = SD_COM_ENSURE_OBJECT_OBJC (holder);
    }
  else if (JSVAL_IS_INT (val))
    {
      ret->type = fcall_type_sint;
      ret->val.sint = JSVAL_TO_INT (val);
    }
  else if (JSVAL_IS_DOUBLE (val))
    {
      jsdouble *dptr = JSVAL_TO_DOUBLE (val);
      ret->type = fcall_type_double;
      ret->val._double = (double) *dptr;
    }
  else if (JSVAL_IS_STRING (val))
    {
      ret->type = fcall_type_string;
      ret->val.string = JS_GetStringBytes (JSVAL_TO_STRING (val));
    }
  else if (JSVAL_IS_BOOLEAN (val))
    {
      ret->type = fcall_type_boolean;
      ret->val.boolean = JSVAL_TO_BOOLEAN (val);
    }
  else
    abort ();

  return YES;
}

void
JSsetVariable (COMobject cObj, const char *variableName, val_t *inval)
{
  nsISupports *_cObj = NS_STATIC_CAST (nsISupports *, cObj);
  nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (_cObj));
  JSObject *jsobj;
  jsval val;
  JSContext *cx = currentJSContext ();

  if (!NS_SUCCEEDED (jsObj->GetJSObject (&jsobj)))
    abort ();

  setJSval (inval, &val);

  if (!JS_SetProperty (cx, jsobj, variableName, &val))
    abort ();
}

swarmITyping *
COM_objc_ensure_object_COM (id oObject)
{
  return NS_STATIC_CAST (swarmITyping *, swarm_directory_objc_ensure_object_COM (oObject));
}

nsresult
COM_objc_ensure_object_COM_return (id oObject, const nsIID *iid, void **ret)
{
  if (oObject)
    return (NS_STATIC_CAST (nsISupports *, swarm_directory_objc_ensure_object_COM (oObject))->QueryInterface (*iid, ret));
  else
    {
      *ret = NULL;
      return NS_OK;
    }
}

swarmITyping *
COM_add_object_COM (swarmITyping *cObject, id oObject)
{
  nsresult rv;
  NS_WITH_SERVICE (nsIXPConnect, xpc, nsIXPConnect::GetCID (), &rv);
  if (!NS_SUCCEEDED (rv))
    abort ();
  
  nsCOMPtr <nsIXPCNativeCallContext> callContext;
  xpc->GetCurrentNativeCallContext (getter_AddRefs (callContext));
  if (!callContext)
    abort ();

  nsCOMPtr <swarmIBase> base (do_QueryInterface (cObject));
  if (!base)
    abort ();
  if (!NS_SUCCEEDED (base->SetNativeCallContext (callContext)))
    abort ();
  return NS_STATIC_CAST (swarmITyping *, swarm_directory_COM_add_object_COM (cObject, oObject));
}
