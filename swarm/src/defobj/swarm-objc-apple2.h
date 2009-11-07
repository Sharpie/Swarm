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
Name:         swarm-objc-apple2.h
Description:  Map Swarm ObjC API to Apple ObjC 2.0 runtime
Library:      defobj
*/

typedef void *retval_t;

#include <objc/runtime.h>

#define _C_IN		'n'
#define _C_INOUT	'N'
#define _C_OUT      	'o'
#define _C_BYCOPY	'O'
#define _C_BYREF	'R'
#define _C_ONEWAY	'V'

#define swarm_class_addIvar(cls, name, size, alignment, types) class_addIvar(cls, name, size, alignment, types)
//#define swarm_class_addMethod(cls, name, imp, types) class_addMethod(cls, name, imp, types)
#define swarm_class_addProtocol(cls, protocol) class_addProtocol(cls, protocol)
#define swarm_class_conformsToProtocol(cls, protocol) class_conformsToProtocol(cls, protocol)
#define swarm_class_copyIvarList(cls, outCount) class_copyIvarList(cls, outCount)
#define swarm_class_copyMethodList(cls, outCount) class_copyMethodList(cls, outCount)
#define swarm_class_copyProtocolList(cls, outCount) class_copyProtocolList(cls, outCount)
#define swarm_class_createInstance(cls, extraBytes) class_createInstance(cls, extraBytes)
#define swarm_class_getClass(cls) object_getClass(cls)
#define swarm_class_getClassMethod(cls, name) class_getClassMethod(cls, name)
#define swarm_class_getInstanceMethod(cls, name) class_getInstanceMethod(cls, name)
#define swarm_class_getInstanceSize(cls) class_getInstanceSize(cls)
#define swarm_class_getInstanceVariable(cls, name) class_getInstanceVariable(cls, name)
#define swarm_class_getMetaclass(cls) objc_getMetaClass(class_getName(cls))
#define swarm_class_getMethodImplementation(cls, sel) class_getMethodImplementation(cls, sel)
#define swarm_class_getName(cls) class_getName(cls)
#define swarm_class_getSuperclass(cls) class_getSuperclass(cls)
#define swarm_class_respondsToSelector(cls, sel) class_respondsToSelector(cls, sel)

#define swarm_objc_allocateClassPair(superClass, name, extraBytes) objc_allocateClassPair(superClass, name, extraBytes)
#define swarm_objc_registerClassPair(cls) objc_registerClassPair(cls)
#define swarm_objc_getClassList(buffer, bufferLen) objc_getClassList(buffer, bufferLen)
#define swarm_objc_lookupClass(name) objc_lookUpClass(name)

#define swarm_object_getClass(obj) object_getClass(obj)
#define swarm_object_setClass(obj, cls) object_setClass(obj, cls)

#define swarm_ivar_getName(ivar) ivar_getName(ivar)
#define swarm_ivar_getOffset(ivar) ivar_getOffset(ivar)
#define swarm_ivar_getTypeEncoding(ivar) ivar_getTypeEncoding(ivar)

#define swarm_method_getImplementation(method) method_getImplementation(method)
#define swarm_method_getName(method) method_getName(method)
#define swarm_method_getTypeEncoding(method) method_getTypeEncoding(method)

// SWARM_TODO - selector type info
#define swarm_sel_getName(sel) sel_getName(sel)
#define swarm_sel_getTypedUid(str, types) sel_getUid(str)
#define swarm_sel_getUid(str) sel_getUid(str)
#define swarm_sel_getUidWithType(str) sel_getUid(str)
#define swarm_sel_isEqual(sel1, sel2) sel_isEqual(sel1, sel2)
#define swarm_sel_registerName(str) sel_registerName(str)
#define swarm_sel_registerTypedName(str, types) sel_registerName(str)

