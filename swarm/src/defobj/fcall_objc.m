// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "fcall_objc.h"
#import <defobj/FArguments.h>
#include <objc/objc.h>

#include <swarmconfig.h>

#ifdef USE_AVCALL
#include <avcall.h>

void
objc_setup_call (FArguments_c *fa, id obj, SEL sel)
{
  av_ptr (fa->objc_avalist, id, obj);
  av_ptr (fa->objc_avalist, SEL, sel);
}

#define OBJC
#include "_fcall.m"
#endif
