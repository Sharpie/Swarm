#include "swarmSelectorImpl.h"

#include "componentIDs.h"
#include "COMsupport.h"
#include "xptinfo.h"
#include "xptcall.h"
#include "xpt_struct.h"

#include <objc/objc-api.h>

NS_IMPL_ISUPPORTS1(swarmSelectorImpl, swarmISelector)

swarmSelectorImpl::swarmSelectorImpl ()
{
  NS_INIT_REFCNT ();
}

swarmSelectorImpl::~swarmSelectorImpl ()
{
}

NS_IMETHODIMP
swarmSelectorImpl::GetPrimaryiid (nsIID **aiid)
{
  *aiid = (nsIID *) &NS_GET_IID (swarmISelector);
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetCid (nsCID **acid)
{
  static NS_DEFINE_CID (cid, SWARM_SELECTOR_CID);
  *acid = (nsCID *) &cid;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::IsBooleanReturn (PRBool *ret)
{
  unsigned i = methodInfo->GetParamCount () - 1;
  const nsXPTParamInfo param = methodInfo->GetParam (i);

  if (param.IsRetval ())
    *ret = param.GetType () == nsXPTType::T_BOOL;
  else
    *ret = PR_FALSE;

  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::IsVoidReturn (PRBool *ret)
{
  unsigned i = methodInfo->GetParamCount () - 1;
  const nsXPTParamInfo param = methodInfo->GetParam (i);

  *ret = !param.IsRetval ();
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetName (char **ret)
{
  *ret = (char *) methodInfo->GetName ();
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetArgCount (unsigned *ret)
{
  *ret = (unsigned) methodInfo->GetParamCount ();
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetArgObjcType (unsigned argIndex, char *retPtr)
{
  const nsXPTParamInfo param = methodInfo->GetParam (argIndex);
  nsXPTType type = param.GetType ();

  char ret;
  
  switch (type)
    {
    case nsXPTType::T_I8:
      ret = _C_CHR;
      break;
    case nsXPTType::T_I16:
      ret = _C_SHT;
      break;
    case nsXPTType::T_I32:
      ret = _C_INT;
      break;
    case nsXPTType::T_I64:
      ret = _C_LNG_LNG;
      break;
    case nsXPTType::T_U8:
      ret = _C_UCHR;
      break;
    case nsXPTType::T_U16:
      ret = _C_USHT;
      break;
    case nsXPTType::T_U32:
      ret = _C_UINT;
      break;
    case nsXPTType::T_U64:
      ret = _C_ULNG_LNG;
      break;
    case nsXPTType::T_FLOAT:
      ret = _C_FLT;
      break;
    case nsXPTType::T_DOUBLE:
      ret = _C_DBL;
      break;
    case nsXPTType::T_CHAR:
      ret = _C_CHR;
      break;
    case nsXPTType::T_VOID:
      ret = _C_VOID;
      break;
    case nsXPTType::T_CHAR_STR:
      ret = _C_CHARPTR;
      break;
    case nsXPTType::T_INTERFACE:
      ret = _C_ID;
      break;

    case nsXPTType::T_BOOL:
    case nsXPTType::T_WCHAR:
    case nsXPTType::T_IID:
    case nsXPTType::T_BSTR:
    case nsXPTType::T_WCHAR_STR:
    case nsXPTType::T_INTERFACE_IS:
    case nsXPTType::T_ARRAY:
    case nsXPTType::T_PSTRING_SIZE_IS:
    case nsXPTType::T_PWSTRING_SIZE_IS:
    default:
      abort ();
    }
  *retPtr = ret;
  return NS_OK;
}
  

NS_IMETHODIMP
swarmSelectorImpl::Create (nsISupports *obj, const char *methodName, swarmISelector **ret)
{
  if (!findMethod (obj, methodName,
                   &methodInterface, &methodIndex, &methodInfo))
    return NS_ERROR_NOT_IMPLEMENTED;
#if 0  
  {
    nsXPTCVariant params[1];
    nsISupports *retInterface = NULL;
    
    params[0].ptr = &retInterface;
    params[0].type = nsXPTType::T_INTERFACE;
    params[0].flags = nsXPTCVariant::PTR_IS_DATA;

    printf ("interface: %p methodIndex: %u methodInfo: %p paramCount: %u\n",
            methodInterface,
            (unsigned) methodIndex,
            methodInfo, 
            (unsigned) methodInfo->GetParamCount ());

    if (!NS_SUCCEEDED (XPTC_InvokeByIndex (methodInterface,
                                           methodIndex,
                                           methodInfo->GetParamCount (),
                                           params)))
      abort ();
    
    printf ("called interface: %p\n", methodInterface);
    {
      swarmITyping *typing;

      if (!NS_SUCCEEDED (methodInterface->QueryInterface (NS_GET_IID (swarmITyping), (void **) &typing)))
        abort ();
      
      id oObj = SD_COM_ENSURE_OBJECT_OBJC (typing);
      
      printf ("typing interface: %p objc object: %p\n", typing, oObj);
      printf ("[%s]\n", (*((struct objc_class **) oObj))->name);
    }
  }
#endif
    
  NS_ADDREF (*ret = NS_STATIC_CAST (swarmISelector*, this));
  return NS_OK;
}
