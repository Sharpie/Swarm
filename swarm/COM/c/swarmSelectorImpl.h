#ifndef __swarmSelectorImpl_h__

#define __swarmSelectorImpl_h__

#include <swarmITyping.h>
#include <swarmISelector.h>
#include "xptinfo.h"
#include "xptcall.h"

class swarmSelectorImpl: public swarmISelector, public swarmITyping
{
public:
  swarmSelectorImpl ();
  virtual ~swarmSelectorImpl ();

  nsISupports *methodInterface;
  PRUint16 methodIndex;
  const nsXPTMethodInfo *methodInfo;
  
  NS_DECL_ISUPPORTS
  NS_DECL_SWARMITYPING
  NS_DECL_SWARMISELECTOR
};

#endif
