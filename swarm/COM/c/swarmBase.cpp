#include "swarmBase.h"

NS_IMETHODIMP
swarmBase::GetWrapper (nsIXPConnectWrappedNative **ret)
{
  NS_ADDREF (*ret = wrapper);
  return NS_OK;
}

NS_IMETHODIMP
swarmBase::SetWrapper (nsIXPConnectWrappedNative *val)
{
  wrapper = val;
  return NS_OK;
}

swarmBase::~swarmBase ()
{
}


  
