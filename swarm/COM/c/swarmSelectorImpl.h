#ifndef __swarmSelectorImpl_h__

#define __swarmSelectorImpl_h__

#include <swarmITyping.h>
#include <swarmISelector.h>
#include "xptinfo.h"
#include "xptcall.h"
#include "jsapi.h"

class swarmSelectorImpl: public swarmISelector, public swarmITyping
{
public:
  swarmSelectorImpl ();
  virtual ~swarmSelectorImpl ();

  unsigned argCount;
  const char *methodName;

  const nsIID *methodIID;
  PRUint16 methodIndex;
  const nsXPTMethodInfo *methodInfo;

  JSFunction *jsFunc;
  unsigned *jsArgTypes;
  unsigned jsReturnType;
  
  NS_DECL_ISUPPORTS
  NS_DECL_SWARMITYPING
  NS_DECL_SWARMISELECTOR

  void setupCOMselector ();
};

#endif
