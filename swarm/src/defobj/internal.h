// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

#import <Swarm/collections.h> // OutputStream
#include <Swarm/misc.h> // size_t
#import <Swarm/swarm-objc-api.h>

externvar id _obj_GCFixedRootZone;

extern size_t alignsizeto (size_t pos, size_t alignment);
extern void *alignptrto (void *ptr, size_t alignment);

extern size_t fcall_type_alignment (fcall_type_t varType);

extern void map_objc_class_ivars (Class class,
                                  void (*process_ivar) (const char *name,
                                                        fcall_type_t type,
                                                        unsigned offset,
                                                        unsigned rank,
                                                        unsigned *dims));

extern void map_object_ivars (id object,
                              void (*process_object) (const char *name,
                                                      fcall_type_t type,
                                                      void *ptr,
                                                      unsigned rank,
                                                      unsigned *dims));
extern ObjcIvar find_ivar (id obj, const char *name);
extern void *ivar_ptr_for_name (id obj, const char *name);

extern void
process_array (unsigned rank,
               unsigned *dims,
               fcall_type_t baseType,
               void (*start_dim) (unsigned dimnum),
               void (*end_dim) (void),
               void (*start_element) (void),
               void (*end_element) (void),
               void (*output_type) (fcall_type_t type,
                                    unsigned offset,
                                    void *data),
               const void *ptr,
               void *data);

void
objc_process_array (const char *type,
                    void (*setup_array) (unsigned rank, unsigned *dims,
                                         fcall_type_t type),
                    void (*start_dim) (unsigned dimnum),
                    void (*end_dim) (void),
                    void (*start_element) (void),
                    void (*end_element) (void),
                    void (*output_type) (fcall_type_t type,
                                         unsigned offset,
                                         void *data),
                    const void *ptr,
                    void *data);
  
extern void
lisp_output_type (fcall_type_t type,
                  const void *ptr,
                  unsigned offset,
                  void *data,
                  id <OutputStream> stream,
                  BOOL deepFlag);

extern void
lisp_process_array (unsigned rank, unsigned *dims, fcall_type_t type,
                    const void *ptr, void *data,
                    id <OutputStream> stream,
                    BOOL deepFlag);

extern const char *lisp_type_for_objc_type (const char *varType, void (*func) (unsigned dim, unsigned count));

extern char *zstrdup (id aZone, const char *str);

extern size_t fcall_type_size (fcall_type_t type);

extern fcall_type_t fcall_type_for_lisp_type (const char *lispTypeString);

extern fcall_type_t fcall_type_for_objc_type (char objcType);


extern const char *objc_type_for_fcall_type (fcall_type_t type);
extern const char *objc_array_subtype (const char *type, unsigned *dims);

extern id type_create (id aZone, const char *typeName);

#if SWARM_OBJC_DONE
extern Class class_copy (Class class);
#endif
extern void class_addVariable (Class class, const char *varName, fcall_type_t varType, unsigned rank, unsigned *dims);
extern const char *class_generate_name (void);

#if SWARM_OBJC_DONE
extern struct objc_ivar_list *ivar_extend_list (struct objc_ivar_list *ivars, unsigned additional);
#endif

extern void object_setVariableFromExpr (id obj, const char *ivarname, id expr);
extern void object_setVariable (id, const char *ivarname, void *ptr);

extern unsigned object_getVariableElementCount (id obj,
                                                const char *ivarName,
                                                fcall_type_t itype,
                                                unsigned irank,
                                                unsigned *idims);

/* TODO: Long double not in objc runtime yet */
#ifndef _C_LNG_DBL
#define _C_LNG_DBL  'X'
#endif
