#import "fcall_objc.h"
#import <defobj/FArguments.h>
#include <objc/objc.h>
#include <avcall.h>

void
objc_setup_call (FArguments_c *fa, id obj, SEL sel)
{
  av_ptr (fa->objc_avalist, id, obj);
  av_ptr (fa->objc_avalist, SEL, sel);
}

#define OBJC
#include "_fcall.m"

