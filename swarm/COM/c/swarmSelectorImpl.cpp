#include "swarmSelectorImpl.h"

#include "componentIDs.h"
#include "COMsupport.h"
#include "xptinfo.h"
#include "xptcall.h"
#include "xpt_struct.h"

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
swarmSelectorImpl::Create (nsISupports *obj, const char *methodName, PRBool objcFlag, swarmISelector **ret)
{
  if (!findMethod (obj, methodName,
                   &methodInterface, &methodIndex, &methodInfo))
    return NS_ERROR_NOT_IMPLEMENTED;
  
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
    
  *ret = NS_STATIC_CAST (swarmISelector*, this);
  return NS_OK;
}
