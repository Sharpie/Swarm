// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h> // OutputStream
#include <misc.h> // size_t
#include <objc/objc-api.h>

#define FCALL_TYPE_COUNT 21

extern size_t alignsizeto (size_t pos, size_t alignment);
extern void *alignptrto (void *ptr, size_t alignment);

extern size_t fcall_type_alignment (fcall_type_t varType);
extern void map_object_ivars (id object,
                              void (*process_object) (const char *name,
                                                      fcall_type_t type,
                                                      void *ptr,
                                                      unsigned rank,
                                                      unsigned *dims));
extern struct objc_ivar *find_ivar (id obj, const char *name);
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

extern fcall_type_t fcall_type_for_lisp_type (const char *lispTypeString);

extern char *zstrdup (id aZone, const char *str);

extern size_t fcall_type_size (fcall_type_t type);
extern fcall_type_t fcall_type_for_objc_type (char objcType);

extern const char *objc_type_for_fcall_type (fcall_type_t type);
extern const char *java_signature_for_fcall_type (fcall_type_t type);

extern id type_create (id aZone, const char *typeName);

extern Class class_copy (Class class);
extern void class_addVariable (Class class, const char *varName, fcall_type_t varType, unsigned rank, unsigned *dims);
extern const char *class_generate_name (void);

extern struct objc_ivar_list *ivar_extend_list (struct objc_ivar_list *ivars, unsigned additional);

extern void object_setVariableFromExpr (id obj, const char *ivarname, id expr);
extern void object_setVariableFromPtr (id, const char *ivarname, types_t *ptr);

