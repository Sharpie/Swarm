// Copyright © 2000, 2001 Swarm Development Group
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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

#include "nsMemory.h" // NS_CI_INTERFACE_GETTER_NAME

#include <defobj.h> // fcall_type_t


NS_IMPL_ISUPPORTS2_CI(swarmSelectorImpl, swarmISelector, swarmITyping)

swarmSelectorImpl::swarmSelectorImpl ()
{
  // NS_INIT_REFCNT ();
}

swarmSelectorImpl::~swarmSelectorImpl ()
{
}

NS_IMETHODIMP
swarmSelectorImpl::GetPrimaryIID (nsIID **aiid)
{
  *aiid = (nsIID *) &NS_GET_IID (swarmISelector);
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetCID (nsCID **acid)
{
  static NS_DEFINE_CID (cid, SWARM_SELECTOR_CID);
  *acid = (nsCID *) &cid;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::GetMethod (const void **ret)

{
  *ret = &method;
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::IsBooleanReturn (PRBool *ret)
{
  if (method.methodInfo)
    {
      PRUint16 count = method.methodInfo->GetParamCount ();
      
      if (count > 0)
        {
          PRUint16 i = count - 1;
          const nsXPTParamInfo param = method.methodInfo->GetParam (i);
          
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
  if (method.methodInfo)
    {
      PRUint16 count = method.methodInfo->GetParamCount ();
      
      if (count > 0)
        {
          PRUint16 i = count - 1;
          const nsXPTParamInfo param = method.methodInfo->GetParam (i);
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
  else if (method.methodInfo)
    ret = methodParamFcallType (method.methodInfo, argIndex);
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
  *ret = method.methodInfo == NULL;
  return NS_OK;
}

void
swarmSelectorImpl::setupCOMselector ()
{
  uint8 paramCount = method.methodInfo->GetParamCount ();
  PRBool voidReturn;
  
  if (!NS_SUCCEEDED (IsVoidReturn (&voidReturn)))
    abort ();
  
  argCount = (unsigned) paramCount;
  if (!voidReturn)
    argCount--;
  methodName = (char *) method.methodInfo->GetName ();
}

NS_IMETHODIMP
swarmSelectorImpl::CreateFromMethod (const void *inmethod,
                                     swarmISelector **ret)
{
  struct method_value *value = (struct method_value *) inmethod;

  method = *value;
  
  jsArgTypes = NULL;

  setupCOMselector ();

  NS_ADDREF (*ret = NS_STATIC_CAST (swarmISelector*, this));
  
  return NS_OK;
}

NS_IMETHODIMP
swarmSelectorImpl::Create (nsISupports *obj,
                           const char *wantedMethodName,
                           swarmISelector **ret)
{
  nsCOMPtr <nsIXPConnectJSObjectHolder> jsObj (do_QueryInterface (obj));

  jsArgTypes = NULL;
 
  if (jsObj)
    {
      JSContext *cx = currentJSContext ();
      argCount = JSmethodArgCount (jsObj, wantedMethodName);
      methodName = JS_strdup (cx, wantedMethodName);
      if (argCount > 0)
        jsArgTypes = (unsigned *) JS_malloc (cx, sizeof (unsigned) * argCount);
      method.methodInfo = NULL;
    }
  else
    {
      if (!findMethod (obj, wantedMethodName,
                       &method.methodIID,
                       &method.methodIndex,
                       &method.methodInfo))
        return NS_ERROR_NOT_IMPLEMENTED;

      setupCOMselector ();
    }

  NS_ADDREF (*ret = NS_STATIC_CAST (swarmISelector*, this));
  return NS_OK;
}

#if 0
NS_IMETHODIMP
swarmSelectorImpl::COMinvokeX (nsISupports *obj, nsXPTCVariant *params)
{
  nsISupports *methodInterface;
  nsresult rv;

  if (!method.methodInfo)
    abort ();

  rv = obj->QueryInterface (*method.methodIID,
                            (void **) &methodInterface);

  if (!NS_SUCCEEDED (rv))
    return rv;

  rv = XPTC_InvokeByIndex (methodInterface,
                           method.methodIndex,
                           method.methodInfo->GetParamCount (),
                           params);
  NS_RELEASE (methodInterface);
  return rv;
}

NS_IMETHODIMP
swarmSelectorImpl::JSinvokeX (nsISupports *obj, jsval *args)
{
  nsCOMPtr <nsIXPConnectJSObjectHolder> jobj (do_QueryInterface (obj));
  JSObject *jsObj;

  if (!jobj)
    abort ();

  if (!NS_SUCCEEDED (jobj->GetJSObject (&jsObj)))
    abort ();
  
  JS_CallFunction (currentJSContext (), jsObj, jsFunc,
                   argCount, args, &args[argCount]);
  return NS_OK;
}
#endif
