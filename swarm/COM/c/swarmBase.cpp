#include "swarmBase.h"

NS_IMETHODIMP
swarmBase::GetNativeCallContext (nsIXPCNativeCallContext **ret)
{
  *ret = nativeCallContext;
  return NS_OK;
}

NS_IMETHODIMP
swarmBase::SetNativeCallContext (nsIXPCNativeCallContext *ret)
{
  nativeCallContext = ret;
  return NS_OK;
}


  
