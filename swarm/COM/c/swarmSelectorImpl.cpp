#include "swarmSelectorImpl.h"

#include "componentIDs.h"
#include "COMsupport.h"
#include "xptinfo.h"
#include "xptcall.h"
#include "xpt_struct.h"

#include <defobj.h> // fcall_type_t

NS_IMPL_ISUPPORTS2(swarmSelectorImpl, swarmISelector, swarmITyping)

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
swarmSelectorImpl::GetArgFcallType (unsigned argIndex, unsigned short *retPtr)
{
  const nsXPTParamInfo& param = methodInfo->GetParam (argIndex);
  const nsXPTType& type = param.GetType ();

  fcall_type_t ret;
  
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

    case nsXPTType::T_WCHAR:
    case nsXPTType::T_IID:
    case nsXPTType::T_BSTR:
    case nsXPTType::T_WCHAR_STR:
    case nsXPTType::T_ARRAY:
    case nsXPTType::T_PSTRING_SIZE_IS:
    case nsXPTType::T_PWSTRING_SIZE_IS:
    default:
      abort ();
    }
  *retPtr = (unsigned) ret;
  return NS_OK;
}
  

NS_IMETHODIMP
swarmSelectorImpl::Create (nsISupports *obj, const char *methodName, swarmISelector **ret)
{
  if (!findMethod (obj, methodName,
                   &methodInterface, &methodIndex, &methodInfo))
    return NS_ERROR_NOT_IMPLEMENTED;
    
  NS_ADDREF (*ret = NS_STATIC_CAST (swarmISelector*, this));
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::Invoke (nsXPTCVariant *params)
{
  return XPTC_InvokeByIndex (methodInterface,
                             methodIndex,
                             methodInfo->GetParamCount (),
                             params);
}
