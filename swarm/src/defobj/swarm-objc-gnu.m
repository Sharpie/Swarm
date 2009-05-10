
#import <defobj/swarm-objc-gnu.h>
#include <objc/Object.h>
#include <objc/Protocol.h>

#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>

extern void __objc_add_class_to_hash(Class class);

//
// Working with classes
//

BOOL
swarm_class_addIvar (Class cls, const char *name, size_t size, uint8_t alignment,
		     const char *types)
{
  if (!cls) return NO;

  IvarList_t ivars = cls->ivars;

  if (ivars) {
    // Reallocate ivar list
    int i, cnt = ivars->ivar_count;
    struct objc_ivar_list *newList = malloc(sizeof(struct objc_ivar_list)
					    + cnt * sizeof(struct objc_ivar));

    // copy old ivars
    for (i = 0; i < cnt; ++i) {
      newList->ivar_list[i].ivar_name = ivars->ivar_list[i].ivar_name;
      newList->ivar_list[i].ivar_type = ivars->ivar_list[i].ivar_type;
      newList->ivar_list[i].ivar_offset = ivars->ivar_list[i].ivar_offset;
    }

    // add the new ivar
    newList->ivar_list[cnt].ivar_name = name;
    newList->ivar_list[cnt].ivar_type = types;
    newList->ivar_list[cnt].ivar_offset = cls->instance_size;

    // add to class
    newList->ivar_count = cnt + 1;
    cls->ivars = newList;
    free(ivars);
  } else {
    // first instance variable
    cls->ivars = malloc(sizeof(struct objc_ivar_list));
    if (!cls->ivars) return NO;
    cls->ivars->ivar_count = 1;
    cls->ivars->ivar_list[0].ivar_name = name;
    cls->ivars->ivar_list[0].ivar_type = types;
    cls->ivars->ivar_list[0].ivar_offset = 0;
  }

  // increase size of class
  cls->instance_size += size;

  return YES;
}

BOOL
swarm_class_addMethod (Class cls, SEL aSel, IMP imp, const char *types)
{
  if (!cls) return NO;

  struct objc_method_list *methodList = cls->methods;
  if (methodList) {
    // Reallocate method list
    int i, size = methodList->method_count;
    struct objc_method_list *newList = malloc(sizeof(struct objc_method_list)
					      + size * sizeof(struct objc_method));

    // copy old methods
    for (i = 0; i < size; ++i) {
      newList->method_list[i].method_name = methodList->method_list[i].method_name;
      newList->method_list[i].method_types = methodList->method_list[i].method_types;
      newList->method_list[i].method_imp = methodList->method_list[i].method_imp;
    }

    // add the new method
    newList->method_list[size].method_name = aSel;
    newList->method_list[size].method_types = types;
    newList->method_list[size].method_imp = imp;

    // add to class
    newList->method_count = size + 1;
    newList->method_next = methodList->method_next;
    cls->methods = newList;
    free(methodList);
  } else {
    cls->methods = malloc(sizeof(struct objc_method_list));
    cls->methods->method_next = NULL;
    cls->methods->method_count = 1;
    cls->methods->method_list[0].method_name = aSel;
    cls->methods->method_list[0].method_types = types;
    cls->methods->method_list[0].method_imp = imp;
  }

  // put in dispatch array
  sarray_at_put_safe (cls->dtable, (size_t) aSel->sel_id, imp);

  return YES;
}

Ivar_t *
swarm_class_copyIvarList (Class cls, unsigned int *outCount)
{
  IvarList_t ivars = cls->ivars;
  Ivar_t *ivarList;
  int i;

  // Get count of all ivars
  *outCount = 0;
  if (ivars == NULL) return NULL;
  *outCount = ivars->ivar_count;
  if (*outCount == 0) return NULL;

  // Allocate ivar array
  ivarList = (Ivar_t *)malloc(*outCount * sizeof(Ivar_t));
  for (i = 0; i < ivars->ivar_count; i++) {
    ivarList[i] = &(ivars->ivar_list[i]);
  }

  return ivarList;
}

void
swarm_class_copyIvars (Class fromClass, Class toClass)
{
  IvarList_t fromIvars = fromClass->ivars;
  int i, count;

  // Any ivars to copy
  if (fromIvars == NULL) return;
  count = fromIvars->ivar_count;
  if (count == 0) return;

  struct objc_ivar_list *newList;
  int start = 0;
  if (toClass->ivars) {
    int cnt = toClass->ivars->ivar_count;
    newList = malloc(sizeof(struct objc_ivar_list)
		     + (count + cnt - 1) * sizeof(struct objc_ivar));

    // copy old ivars
    for (i = 0; i < cnt; ++i) {
      newList->ivar_list[i].ivar_name = toClass->ivars->ivar_list[i].ivar_name;
      newList->ivar_list[i].ivar_type = toClass->ivars->ivar_list[i].ivar_type;
      newList->ivar_list[i].ivar_offset = toClass->ivars->ivar_list[i].ivar_offset;
    }
    start = cnt;
  } else {
    newList = malloc(sizeof(struct objc_ivar_list)
		     + count * sizeof(struct objc_ivar));
  }

  // copy new ivars
  for (i = 0; i < count; ++i) {
    newList->ivar_list[start + i].ivar_name = fromClass->ivars->ivar_list[i].ivar_name;
    newList->ivar_list[start + i].ivar_type = fromClass->ivars->ivar_list[i].ivar_type;
    newList->ivar_list[start + i].ivar_offset = fromClass->ivars->ivar_list[i].ivar_offset;
  }

  // add to class
  toClass->ivars = newList;
  toClass->instance_size += fromClass->instance_size - fromClass->ivars->ivar_list[0].ivar_offset;
}


Method **
swarm_class_copyMethodList (Class cls, unsigned int *outCount)
{
  struct objc_method_list *methods;
  Method **methodList;
  int i, j;

  // Get count of all methods
  *outCount = 0;
  methods = cls->methods;
  while (methods != NULL) {
    *outCount += methods->method_count;
    methods = methods->method_next;
  }

  // no methods?
  if (*outCount == 0) return NULL;

  // Allocate method array
  methodList = (Method **)malloc(*outCount * sizeof(Method *));
  i = 0;
  methods = cls->methods;
  while (methods != NULL) {
    for (j = 0; j < methods->method_count; ++j) {
      methodList[i] = &(methods->method_list[j]);
      ++i;
    }
    methods = methods->method_next;
  }

  return methodList;
}

BOOL
swarm_class_addProtocol (Class cls, Protocol *protocol)
{
  if (!cls) return NO;
  if (!protocol) return NO;

  // We could reallocate the protocol list
  // but in general not many protocols

  // allocate the protocol list
  struct objc_protocol_list *newP = malloc (sizeof(struct objc_protocol_list));
  newP->list[0] = protocol;
  newP->count = 1;

  // put it in the protocol chain
  newP->next = cls->protocols;
  cls->protocols = newP;

  return YES;
}

Protocol **
swarm_class_copyProtocolList (Class cls, unsigned int *outCount)
{
  struct objc_protocol_list *protocols;
  Protocol **protocolList;
  int i, j;

  // Get count of all protocols
  *outCount = 0;
  protocols = cls->protocols;
  while (protocols != NULL) {
    *outCount += protocols->count;
    protocols = protocols->next;
  }

  // no protocols?
  if (*outCount == 0) return NULL;

  // Allocate protocol array
  protocolList = (Protocol **)malloc(*outCount * sizeof(Protocol *));
  i = 0;
  protocols = cls->protocols;
  while (protocols != NULL) {
    for (j = 0; j < protocols->count; ++j) {
      protocolList[i] = protocols->list[j];
      ++i;
    }
    protocols = protocols->next;
  }

  return protocolList;
}

Ivar_t
swarm_class_getInstanceVariable (Class cls, const char *name)
{
  unsigned i;

  if (!cls) return NULL;
  IvarList_t ivars = cls->ivars;

  // no ivars
  if (ivars == NULL) return NULL;
  if (ivars->ivar_count == 0) return NULL;

  for (i = 0; i < ivars->ivar_count; i++) {
    //printf("%s\n", ivars->ivar_list[i].ivar_name);
    if (strcmp (ivars->ivar_list[i].ivar_name, name) == 0)
      return &(ivars->ivar_list[i]);
  }

  return NULL;
}

BOOL
swarm_class_respondsToSelector (Class cls, SEL sel)
{
  if (class_get_instance_method(cls, sel)) return YES;
  else if (class_get_class_method(cls->class_pointer, sel)) return YES;
  else return NO;
}

BOOL
swarm_class_conformsToProtocol (Class cls, Protocol *protocol)
{
  int i;
  struct objc_protocol_list* proto_list;
  id parent;

  for (proto_list = cls->protocols;
       proto_list; proto_list = proto_list->next)
    {
      for (i=0; i < proto_list->count; i++)
      {
        if ([proto_list->list[i] conformsTo: protocol])
          return YES;
      }
    }

  if ((parent = cls->super_class))
    return [parent conformsTo: protocol];
  else
    return NO;
}

//
// Adding classes
//

Class
swarm_objc_allocateClassPair (Class superClass, const char *name,
			      size_t extraBytes)
{
  Class meta_class;
  Class super_class;
  Class new_class;
  Class root_class;

#if SWARM_OBJC_DEBUG
  printf("Allocating class pair: %s\n", name);
#endif

  // SWARM_OBJC_TODO - Allow creation of root class

  //
  // Ensure that the superclass exists and that someone
  // hasn't already implemented a class with the same name
  //
  super_class = objc_lookup_class (superClass->name);
  if (super_class == nil) return nil;
  if (objc_lookup_class (name) != nil) return nil;

  // Find the root class
  root_class = super_class;
  while (root_class->super_class != nil) {
    root_class = root_class->super_class;
  }

  // Allocate space for the class and its metaclass
  new_class = calloc (2, sizeof(struct objc_class));
  meta_class = &new_class[1];

  // setup class
  new_class->class_pointer = meta_class;
  new_class->info = _CLS_CLASS;
  meta_class->info = _CLS_META;
  CLS_SETRESOLV(new_class);
  CLS_SETRESOLV(meta_class);
  new_class->instance_size = super_class->instance_size + extraBytes;
  meta_class->instance_size = sizeof(struct objc_class);

  // Create a copy of the class name.
  new_class->name = malloc (strlen (name) + 1);
  meta_class->name = malloc (strlen (name) + 1);
  strcpy ((char*)new_class->name, name);
  strcpy ((char*)meta_class->name, name);

  // We can add methods later.
  new_class->methods = NULL;
  meta_class->methods = NULL;

  // dispatch table
  new_class->dtable = sarray_lazy_copy (super_class->dtable);
  meta_class->dtable = sarray_lazy_copy (super_class->class_pointer->dtable);

  //
  // Connect the class definition to the class hierarchy:
  // Connect the class to the superclass.
  // Connect the metaclass to the metaclass of the superclass.
  // Connect the metaclass of the metaclass to
  //      the metaclass of the root class.
  new_class->super_class = super_class;
  meta_class->super_class = super_class->class_pointer;
  meta_class->class_pointer = (void *)root_class->class_pointer;

  // Finally, register the class with the runtime.
#if SWARM_OBJC_DEBUG
  printf("Adding class to runtime: %s\n", new_class->name);
#endif
  __objc_add_class_to_hash(new_class);

  return new_class;
}

Class
swarm_objc_allocateClassPairCopy (Class cls, const char *name,
				  size_t extraBytes)
{
  printf("swarm_objc_allocateClassPairCopy() not implemented\n");
  abort();
}

void
swarm_objc_registerClassPair (Class cls)
{
}

int
swarm_objc_getClassList (Class *buffer, int bufferLen)
{
  Class class;
  void *enumState;
  int nclasses = 0;

  for (enumState = NULL; (class = objc_next_class (&enumState)); nclasses++) {
    if ((buffer != NULL) && (nclasses < bufferLen)) buffer[nclasses] = class;
  }

  return nclasses;
}

//
// Working with instances
//
Class
swarm_object_setClass (id obj, Class cls)
{
  Class old = obj->class_pointer;
  obj->class_pointer = cls;
  return old;
}

//
// Working with protocols
//
