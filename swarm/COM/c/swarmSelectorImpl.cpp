#include "swarmSelectorImpl.h"

#include "componentIDs.h"
#include "COMsupport.h"

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
  printf ("methodName: `%s' objcFlag: %u\n", methodName, (unsigned) objcFlag);
  findMethod (obj, methodName);
  *ret = NS_STATIC_CAST (swarmISelector*, this);
  return NS_OK;
}
