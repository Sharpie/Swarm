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

#ifndef __swarmSelectorImpl_h__

#define __swarmSelectorImpl_h__

#include <swarmITyping.h>
#include <swarmISelector.h>
#include "xptinfo.h"
#include "xptcall.h"
#include "jsapi.h"
#include "COMsupport.h" // method_value

class swarmSelectorImpl: public swarmISelector, public swarmITyping
{
public:
  swarmSelectorImpl ();
  virtual ~swarmSelectorImpl ();

  unsigned argCount;
  const char *methodName;

  struct method_value method;

  JSFunction *jsFunc;
  unsigned *jsArgTypes;
  unsigned jsReturnType;
  
  NS_DECL_ISUPPORTS
  NS_DECL_SWARMITYPING
  NS_DECL_SWARMISELECTOR

  void setupCOMselector ();
};

#endif
