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
      ret = JSToFcallType (type);
    }
  else if (methodInfo)
    ret = methodParamFcallType (methodInfo, argIndex);
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
swarmSelectorImpl::SetIntegerArg (unsigned argIndex)
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
swarmSelectorImpl::SetIntegerReturn ()
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
      nsIID *methodIID;

      if (!findMethod (obj, wantedMethodName,
                       &methodIID, &methodIndex, &methodInfo))
        return NS_ERROR_NOT_IMPLEMENTED;
      
      if (!NS_SUCCEEDED (obj->QueryInterface (*methodIID, (void **) &methodInterface)))
        abort ();
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
