/* This file contains the implementation of class Protocol.
   Copyright (C) 1993 Free Software Foundation, Inc.

This file is part of GNU CC. 

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
 
/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "objc/Protocol.h"
#include "objc/objc-api.h"

/* Method description list */
struct objc_method_description_list {
        int count;
        struct objc_method_description list[1];
};


@implementation Protocol
{
@private
        char *protocol_name;
        struct objc_protocol_list *protocol_list;
        struct objc_method_description_list *instance_methods, *class_methods; 
}

/* Obtaining attributes intrinsic to the protocol */

- (const char *)name
{
  return protocol_name;
}

/* Testing protocol conformance */

- (BOOL) conformsTo: (Protocol *)aProtocolObject
{
  int i;
  struct objc_protocol_list* proto_list;

  if (aProtocolObject == nil)
    return NO;

  if (!strcmp(aProtocolObject->protocol_name, self->protocol_name))
    return YES;

  for (proto_list = protocol_list; proto_list; proto_list = proto_list->next)
    {
      for (i=0; i < proto_list->count; i++)
	{
	  if ([proto_list->list[i] conformsTo: aProtocolObject])
	    return YES;
	}
    }

  return NO;
}

typedef
struct {
  @defs(Protocol);
}
*pcl;

struct objc_method_description * descriptionForInstanceMethod(pcl self, SEL aSel)
{
  int i;
  struct objc_protocol_list* proto_list;
  const char* name = sel_get_name (aSel);
  struct objc_method_description *result;

  if (self->instance_methods)
    for (i = 0; i < self->instance_methods->count; i++)
      {
        if (!strcmp ((char*)self->instance_methods->list[i].name, name))
  	    return &(self->instance_methods->list[i]);
      }

  for (proto_list = self->protocol_list; proto_list; proto_list = proto_list->next)
    {
      for (i=0; i < proto_list->count; i++)
	{
	  if ((result = descriptionForInstanceMethod((pcl)proto_list->list[i], aSel)))
	    return result;
	}
    }

  return NULL;
}

struct objc_method_description * descriptionForClassMethod(pcl self, SEL aSel)
{
  int i;
  struct objc_protocol_list* proto_list;
  const char* name = sel_get_name (aSel);
  struct objc_method_description *result;

  if (self->class_methods)
    for (i = 0; i < self->class_methods->count; i++)
      {
        if (!strcmp ((char*)self->class_methods->list[i].name, name))
  	  return &(self->class_methods->list[i]);
      }

  for (proto_list = self->protocol_list; proto_list; proto_list = proto_list->next)
    {
      for (i=0; i < proto_list->count; i++)
	{
	  if ((result = descriptionForClassMethod((pcl)proto_list->list[i], aSel)     ))
	    return result;
	}
    }

  return NULL;
}

/* Looking up information specific to a protocol */

- (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel
{
  return descriptionForInstanceMethod(self, aSel);
}

- (struct objc_method_description *) descriptionForClassMethod:(SEL)aSel;
{
  return descriptionForClassMethod(self, aSel);
}

@end
