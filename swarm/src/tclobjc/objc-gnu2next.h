/* Definitions to allow compilation of GNU objc code NeXTSTEP machines
   Copyright (C) 1993 Free Software Foundation, Inc.

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   This file is part of the GNU Objective-C Collection library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

/* This file is by no means complete. */

#ifndef __objc_gnu2next_h_INCLUDE_GNU
#define __objc_gnu2next_h_INCLUDE_GNU

#ifdef NeXT

#include <objc/objc-class.h>
#define arglist_t marg_list
#define Method_t Method
#define TypedStream NXTypedStream

#define class_pointer isa

#define objc_write_type(STREAM, TYPE, VAR) \
     NXWriteType(STREAM, TYPE, VAR)
#define objc_write_types(STREAM, TYPE, args...) \
     NXWriteTypes(STREAM, TYPE, args)
#define objc_write_object(STREAM, VAR) \
     NXWriteObject(STREAM, VAR)
#define objc_write_object_reference(STREAM, VAR) \
     NXWriteObjectReference(STREAM, VAR)
#define objc_read_type(STREAM, TYPE, VAR) \
     NXReadType(STREAM, TYPE, VAR)
#define objc_read_types(STREAM, TYPE, args...) \
     NXReadTypes(STREAM, TYPE, args)
#define objc_read_object(STREAM, VAR) \
     do { (*(VAR)) = NXReadObject(STREAM); } while (0)
#define objc_write_root_object \
     NXWriteRootObject
#define objc_open_typed_stream_for_file \
    NXOpenTypedStreamForFile
#define objc_close_typed_stream NXCloseTypedStream

#define objc_msg_lookup(OBJ,SEL) \
 [(id)(OBJ) methodFor:(SEL)]

#define class_create_instance(CLASS) class_createInstance(CLASS, 0)
#define sel_get_name(ASEL) sel_getName(ASEL)
#define sel_get_uid(METHODNAME) sel_getUid(METHODNAME)
#define class_get_instance_method(CLASSPOINTER, SEL) \
     class_getInstanceMethod(CLASSPOINTER, SEL)
#define class_get_class_method(CLASSPOINTER, SEL) \
     class_getClassMethod(CLASSPOINTER, SEL)
#define method_get_sizeof_arguments(METHOD) \
     method_getSizeOfArguments(METHOD)
#define objc_lookup_class(NAME) objc_lookUpClass(NAME)

#define CLS_ISCLASS(cls) ((cls)&&CLS_GETINFO(cls, CLS_CLASS))

#define METHOD_NULL NULL

#define OBJC_READONLY 1
#define OBJC_WRITEONLY 2

#endif /* NeXT */

#endif /* __objc_gnu2next_h_INCLUDE_GNU */
