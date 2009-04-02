// Swarm library. Copyright © 2008 Swarm Development Group.
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

/*
Name:         swarm-objc-apple.m
Description:  Map Swarm ObjC API to Apple ObjC runtime
Library:      defobj
*/


#include <objc/objc-api.h>

#if defined(OBJC_API_VERSION) && OBJC_API_VERSION >= 2

// Apple ObjC 2.0
#import <defobj/swarm-objc-apple2.h>

Class
swarm_objc_allocateClassPairCopy (Class cls, const char *name,
				  size_t extraBytes)
{
  printf("swarm_objc_allocateClassPairCopy() not implemented\n");
  abort();
}

const char *
swarm_sel_getTypeEncoding (SEL sel)
{
  printf("swarm_sel_getTypeEncoding() not implemented\n");
  abort();
}

BOOL
swarm_class_addMethod (Class cls, SEL aSel, IMP imp, const char *types)
{
	printf("class_addMethod: %s, %s\n", class_getName(cls), sel_getName(aSel));
	BOOL ret = class_addMethod(cls, aSel, imp, types);
	if (!ret) printf("error adding method\n");
	return ret;
}

void
swarm_class_copyIvars (Class fromClass, Class toClass)
{
	unsigned int i, outCount;
	unsigned int aSize, anAlign;

	// copy the instance variables from one class to the other
	Ivar *ivarList = class_copyIvarList(fromClass, &outCount);
	if (ivarList) {
		for (i = 0; i < outCount; ++i) {
			NSGetSizeAndAlignment(ivar_getTypeEncoding(ivarList[i]), &aSize, &anAlign);
			printf("copy ivar: %s %d %d\n", ivar_getName(ivarList[i]), aSize, log2(aSize));
			class_addIvar(toClass, ivar_getName(ivarList[i]), aSize, log2(aSize),
				ivar_getTypeEncoding(ivarList[i]));
		}
		free(ivarList);
	}
}

#else

// Apple ObjC 1.0
#import <defobj/swarm-objc-apple.h>

#endif
