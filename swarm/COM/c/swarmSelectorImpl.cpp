#include "swarmSelectorImpl.h"

#include "componentIDs.h"
#include "COMsupport.h"
#include "xptinfo.h"
#include "xptcall.h"
#include "xpt_struct.h"

#include "nsCOMPtr.h"
#include "nsIXPConnect.h"
#include "jsapi.h"
#define VOID_TYPE 7

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
  if (methodInfo)
    {
      PRUint16 count = methodInfo->GetParamCount ();
      
      if (count > 0)
        {
          PRUint16 i = count - 1;
          const nsXPTParamInfo param = methodInfo->GetParam (i);
          
          if (param.IsRetval ())
            *ret = param.GetType () == nsXPTType::T_BOOL;
          else
            *ret = PR_FALSE;
        }
      else
        *ret = PR_FALSE;
    }
  else
    *ret = jsReturnType == JSVAL_BOOLEAN;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::IsVoidReturn (PRBool *ret)
{
  if (methodInfo)
    {
      PRUint16 count = methodInfo->GetParamCount ();
      
      if (count > 0)
        {
          PRUint16 i = count - 1;
          const nsXPTParamInfo param = methodInfo->GetParam (i);
          *ret = !param.IsRetval ();
        }
      else
        *ret = PR_TRUE;
    }
  else
    *ret = jsReturnType == VOID_TYPE;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetName (char **ret)
{
  *ret = (char *) methodName;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetArgCount (unsigned *ret)
{  
  *ret = argCount;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetArgFcallType (unsigned argIndex, unsigned short *retPtr)
{
  fcall_type_t ret = fcall_type_void;
  
  if (jsArgTypes)
    {
      unsigned type = (argIndex == argCount
                       ? jsReturnType
                       : jsArgTypes[argIndex]);
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
    }
  else if (methodInfo)
    {
      const nsXPTParamInfo& param = methodInfo->GetParam (argIndex);
      const nsXPTType& type = param.GetType ();
      
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
    }
  *retPtr = (unsigned) ret;
  return NS_OK;
}
  
NS_IMETHODIMP
swarmSelectorImpl::SetObjectArg (unsigned argIndex)
{
  jsArgTypes[argIndex] = JSVAL_OBJECT;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetIntArg (unsigned argIndex)
{
  jsArgTypes[argIndex] = JSVAL_INT;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetDoubleArg (unsigned argIndex)
{
  jsArgTypes[argIndex] = JSVAL_DOUBLE;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetStringArg (unsigned argIndex)
{
  jsArgTypes[argIndex] = JSVAL_STRING;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetBooleanArg (unsigned argIndex)
{
  jsArgTypes[argIndex] = JSVAL_BOOLEAN;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetObjectReturn ()
{
  jsReturnType = JSVAL_OBJECT;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetIntReturn ()
{
  jsReturnType = JSVAL_INT;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetDoubleReturn ()
{
  jsReturnType = JSVAL_DOUBLE;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetStringReturn ()
{
  jsReturnType = JSVAL_STRING;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetBooleanReturn ()
{
  jsReturnType = JSVAL_BOOLEAN;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::SetVoidReturn ()
{
  jsReturnType = VOID_TYPE;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::IsJavaScript (PRBool *ret)
{
  *ret = methodInfo == NULL;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::Create (nsISupports *obj,
                           const char *wantedMethodName,
                           swarmISelector **ret)
{
  nsCOMPtr <nsIXPConnectJSObjectHolder> jobj (do_QueryInterface (obj));

  jsArgTypes = NULL;
 
  if (jobj)
    {
      if (!NS_SUCCEEDED (jobj->GetJSObject (&jsObj)))
        abort ();

      JSContext *cx = currentJSContext ();
      JS_AddRoot (cx, &jsObj);
      jsval funcVal;

      if (!JS_GetProperty (cx, jsObj, wantedMethodName, &funcVal))
        return NS_ERROR_NOT_IMPLEMENTED;

      jsFunc = JS_ValueToFunction (cx, funcVal);

      jsval arityVal;

      if (!JS_GetProperty (cx, JSVAL_TO_OBJECT (funcVal), "arity", &arityVal))
        abort ();

      argCount = JSVAL_TO_INT (arityVal);
      methodName = JS_strdup (cx, wantedMethodName);
      if (argCount > 0)
        jsArgTypes = (unsigned *) JS_malloc (cx, sizeof (unsigned) * argCount);
      methodInfo = NULL;
    }
  else
    {
      if (!findMethod (obj, wantedMethodName,
                       &methodInterface, &methodIndex, &methodInfo))
        return NS_ERROR_NOT_IMPLEMENTED;
      
      uint8 paramCount = methodInfo->GetParamCount ();
      PRBool voidReturn;
      
      if (!NS_SUCCEEDED (IsVoidReturn (&voidReturn)))
        abort ();
      
      argCount = (unsigned) paramCount;
      if (!voidReturn)
        argCount--;
      methodName = (char *) methodInfo->GetName ();
    }

  NS_ADDREF (*ret = NS_STATIC_CAST (swarmISelector*, this));
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::COMinvoke (nsXPTCVariant *params)
{
  if (!methodInfo)
    abort ();

  return XPTC_InvokeByIndex (methodInterface,
                             methodIndex,
                             methodInfo->GetParamCount (),
                             params);
}

NS_IMETHODIMP
swarmSelectorImpl::JSinvoke (jsval *args)
{
  JS_CallFunction (currentJSContext (), jsObj, jsFunc,
                   argCount, args, &args[argCount]);
  return NS_OK;
}
